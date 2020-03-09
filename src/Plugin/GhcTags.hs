{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.GhcTags ( plugin ) where

import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS
#if __GLASGOW_HASKELL__ < 808
import           Data.Functor ((<$))
#endif
import           Data.List (sortBy)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error  (tryIOError)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Encoding as Text

import           GhcPlugins ( CommandLineOption
                            , Hsc
                            , HsParsedModule (..)
                            , Located
                            , ModSummary
                            , Plugin (..)
                            )
import qualified GhcPlugins
import           HsExtension (GhcPs)
import           HsSyn (HsModule)

import           Plugin.GhcTags.Generate
import           Plugin.GhcTags.Tag
import qualified Plugin.GhcTags.Vim as Vim


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
      pluginRecompile    = GhcPlugins.purePlugin
   }


-- | IOExcption wrapper; it is useful for the user to know that it's the plugin
-- not `ghc` that throwed the error.
--
data GhcTagsPluginException =
    GhcTagsPluginIOExceptino IOException
    deriving Show

instance Exception GhcTagsPluginException


-- | The plugin does not change the 'HsParedModule', it only runs side effects.
--
ghcTagPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
ghcTagPlugin options _modSummary hsParsedModule@HsParsedModule {hpm_module} =
    hsParsedModule <$ GhcPlugins.liftIO (updateTags tagsFile hpm_module)
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
    -- wrap 'IOException's
    handle (throwIO . GhcTagsPluginIOExceptino) $

      -- Take exclusive lock.  This assures that only one thread will have access to
      -- the tags file.  Parsing and the rest of the compilation pipeline can
      -- happen concurrently.
      mvarLock tagsMVar $ \mTagsMap -> do
        (tagsMap :: TagsMap) <-
          case mTagsMap of

            Just tagsMap -> return tagsMap

            -- the 'tagsMVar' is empty, which means we are compiling the first
            -- module.  In this case read the tags from disk.
            Nothing -> do
              a <- doesFileExist tagsFile
              res <-
                if a
                  then do
                    mtext <- tryIOError (Text.decodeUtf8 <$> BS.readFile tagsFile)
                    case mtext of
                      Left err    -> do
                        putStrLn $ "GhcTags: error reading \"" ++ tagsFile ++ "\": " ++ (show err)
                        pure $ Right []
                      Right txt ->
                        Vim.parseTagsFile txt
                  else pure $ Right []
              case res of
                Left err -> do
                  putStrLn $ "GhcTags: error reading or parsing \"" ++ tagsFile ++ "\": " ++ err
                  return $ Map.empty
                Right tagList -> do
                  return $ mkTagsMap tagList

        cwd <- getCurrentDirectory
        -- absolute directory path of the tags file
        -- we need absolute path to make all tags file relative to it.
        tagsDir <- makeAbsolute (fst $ splitFileName tagsFile)

        let tagsMap' :: TagsMap
            tagsMap' =
                (mkTagsMap               -- created 'TagsMap'
                  . map (fixFileName cwd tagsDir)
                                         -- fix file names
                  . mapMaybe ghcTagToTag -- tranalte 'GhcTag' to 'Tag'
                  . getGhcTags           -- generate 'GhcTag's
                  $ lmodule)
              `Map.union`
                tagsMap
                
        -- update tags file, this will force evaluation `tagsMap'`, so when we
        -- write it to `tagsMVar' it will not contain any thunks.
        withFile tagsFile WriteMode
          $ flip BS.hPutBuilder
              ( Vim.formatTagsFile
              . sortBy compareTags
              . concat
              . Map.elems
              $ tagsMap'
              )

        pure (Just tagsMap')

  where
    fixFileName :: FilePath -> FilePath -> Tag -> Tag
    fixFileName cwd tagsDir tag@Tag { tagFile = TagFile path } =
      tag { tagFile = TagFile (makeRelative tagsDir (cwd </> path)) }


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
