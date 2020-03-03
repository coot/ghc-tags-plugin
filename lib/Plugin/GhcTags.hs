{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- only to get hash of the current commit
{-# LANGUAGE TemplateHaskell     #-}

module Plugin.GhcTags ( plugin ) where

import           Control.Concurrent
import           Control.Exception
import           Data.Bool (bool)
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Functor ((<$))
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Version (showVersion)
-- import           Data.Foldable (traverse_)
import           System.IO
import           System.IO.Error  (tryIOError)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Directory
import           Text.Printf (printf)

import qualified Development.GitRev as GitRev

import           GhcPlugins ( CommandLineOption
                            , Hsc
                            , HsParsedModule (..)
                            , ModSummary
                            , Plugin (..)
                            , liftIO
                            , purePlugin
                            )
import           GhcPlugins hiding (occName, (<>))
import           HsExtension (GhcPs)
import           HsSyn (HsModule)

import           Plugin.GhcTags.Generate
import           Plugin.GhcTags.Parser

import           Paths_ghc_tags_plugin (version)


-- |  Global shared state which persists across compilation of different
-- modules - a nasty hack which is only used for optimzation.
--
tagsMVar :: MVar (Maybe TagsMap)
tagsMVar = unsafePerformIO $ newMVar Nothing

-- | The GhcTags plugin.  It will run for every compiled module and have access
-- to parsed syntax tree.  It will inpect it and:
--
-- * update a global mutable state variable, which stores a tag map.
--   It is shared accross modules compiled in the same `ghc` run.
-- * update 'tags' file.
--
-- The global mutable variable save us from parsing the tags file for every
-- compiled module.
--
-- __The syntax tree is left unchanged.__
-- 
-- The tags file will contain location information about:
--
--  * top level terms
--  * data types
--  * record fields
--  * type synonyms
--  * type classes
--  * type class members
--  * type class instances
--  * type families                           /(standalone and associated)/
--  * type family instances                   /(standalone and associated)/
--  * data type families                      /(standalone and associated)/
--  * data type families instances            /(standalone and associated)/
--  * data type family instances constructors /(standalone and associated)/
--
plugin :: Plugin
plugin = GhcPlugins.defaultPlugin {
      parsedResultAction = ghcTagPlugin,
      pluginRecompile    = purePlugin
   }


-- | The plugin does not change the 'HsParedModule', it only runs side effects.
--
ghcTagPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
ghcTagPlugin options _modSummary hsParsedModule@HsParsedModule {hpm_module} =
    hsParsedModule <$ liftIO (updateTags tagsFile hpm_module)
  where
    tagsFile :: FilePath
    tagsFile = case options of
      []    -> "tags"
      a : _ -> a


-- | Extract tags from a module and update tags file as well as the 'tagsMVar'
-- Using 'tagsMVar' we can save on parsing the tags file: we do it only when
-- the first module is compiled.  We need to write the results at every
-- compilation step since we don't know if the currently compiled module is the
-- last one or not.
--
updateTags :: FilePath
           -> Located (HsModule GhcPs)
           -> IO ()
updateTags tagsFile lmodule =
    -- Take exclusive lock.  This assures that only one thread will have access to
    -- the tags file.  Parsing and the rest of the compilation pipeline can
    -- happen concurrently.
    mvarLock tagsMVar $ \mTagsMap -> do
      (tagsMap :: TagsMap) <-
        case mTagsMap of

          Nothing -> do
            a <- doesFileExist tagsFile
            res <-
              if a
                then do
                  mbytes <- tryIOError (BS.readFile tagsFile)
                  case mbytes of
                    Left err    -> do
                      putStrLn $ "GhcTags: error reading \"" ++ tagsFile ++ "\": " ++ (show err)
                      return $ Right []
                    Right bytes ->
                      parseVimTagFile bytes
                else return $ Right []
            case res of
              Left err -> do
                putStrLn $ "GhcTags: error reading or parsing \"" ++ tagsFile ++ "\": " ++ err
                return $ Map.empty
              Right tagList -> do
                return $ mkTagsMap tagList

          Just tagsMap -> return tagsMap

      let tagsMap', updatedTagsMap :: TagsMap
          tagsMap' =
              mkTagsMap
            $ mapMaybe ghcTagToTag
            $ generateTagsForModule
            $ lmodule

          updatedTagsMap = tagsMap' `Map.union` tagsMap

      {-
      putStrLn $ "tags found"
      traverse_ print
        $ sortOn tagName
        $ concat
        $ Map.elems tagsMap'
      -}

      -- update 'tagsIORef', make sure that `updateTagsMap` is evaluated.
      -- TODO: this is not attomic, which will break when compiling multiple
      -- modules at the same time.  I think we need to use 'MVar' and
      -- 'takeMVar'.

      -- update tags file
      withFile tagsFile WriteMode $ \fhandle ->
        BS.hPutBuilder fhandle $
             BS.stringUtf8 (formatHeader "TAG_FILE_SORTED"    "1")
          <> BS.stringUtf8 (formatHeader "TAG_FILE_ENCODING"  "utf-8")
          <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_AUTHOR" "Marcin Szamotulski")
          <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_NAME"   "ghc-tags-pluginn")
          <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_URL"
                                          "https://hackage.haskell.org/package/ghc-tags-plugin")
          <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_VERSION"
                                          (showVersion version ++ " (" ++ gitExtra ++ ")"))
          <> foldMap formatVimTag
              (sort $ concat $ Map.elems updatedTagsMap)

      return $ updatedTagsMap `seq` Just updatedTagsMap
  where
    formatHeader :: String -> String -> String
    formatHeader header arg = printf ("!_" ++ header ++ "\t%s\t\n")  arg

    gitExtra :: String
    gitExtra = $(GitRev.gitHash) ++ bool "" " DIRTY" $(GitRev.gitDirty)


-- | The 'MVar' is used as an exlusive lock.  Also similar to 'bracket' but
-- updates the 'MVar' with returned value, or put the original value if an
-- exception is thrown by the continuation (or an async exception).
--
mvarLock :: MVar a
         -> (a -> IO a)
         -> IO ()
mvarLock v k = mask $ \unmask -> do
    a <- takeMVar v
    a' <- unmask (k a)
          `onException`
          putMVar v a
    putMVar v $! a'


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { gtSrcSpan, gtTag, gtKind } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName = TagName (fs_bs gtTag)
                   , tagFile = TagFile (fs_bs (srcSpanFile realSrcSpan))
                   , tagLine = srcSpanStartLine realSrcSpan
                   , tagKind = Just gtKind
                   }


formatVimTag :: Tag -> Builder
formatVimTag Tag { tagName, tagFile, tagLine, tagKind } =
       BS.byteString (getTagName tagName)
    <> BS.charUtf8 '\t'
    <> BS.byteString (getTagFile tagFile)
    <> BS.charUtf8 '\t'
    <> BS.intDec tagLine
    <> case tagKind of
        Just k ->
             BS.stringUtf8 ";\"\t"
          <> BS.charUtf8 (tagKindToChar k)
        Nothing -> mempty
    <> BS.charUtf8 '\n'
