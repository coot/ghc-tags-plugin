{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.GhcTags ( plugin ) where

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.IORef
import           Data.Functor ((<$))
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
-- import           Data.Foldable (traverse_)
import           System.IO
import           System.IO.Error  (tryIOError)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Directory

import           GhcPlugins ( CommandLineOption
                            , Hsc
                            , HsParsedModule (..)
                            , ModSummary
                            , Plugin (..)
                            , liftIO
                            )
import           GhcPlugins hiding (occName, (<>))
import           HsExtension (GhcPs)
import           HsSyn (HsModule)

import           Plugin.GhcTags.Generate
import           Plugin.GhcTags.Parser


-- | IORef which is shared across various compilations - a nasty hack which is
-- only used for optimzation.
--
tagsIORef :: IORef (Maybe TagsMap)
tagsIORef = unsafePerformIO $ newIORef Nothing

-- | The GhcTags plugin.
--
plugin :: Plugin
plugin = GhcPlugins.defaultPlugin { parsedResultAction = ghcTagPlugin }


-- the GhcTags plugin does not change the 'HsParedModule', it only runs side
-- effects.
--
ghcTagPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
ghcTagPlugin options _modSummary hsParsedModule@HsParsedModule {hpm_module} =
    hsParsedModule <$ liftIO (updateTags tagsFile hpm_module)
  where
    tagsFile :: FilePath
    tagsFile = case options of
      []    -> "tags"
      a : _ -> a


-- | Extract tags from a module and update tags file as well as the 'tagsIORef'
-- Using 'tagsIORef' we can save on parsing the tags file: we do it only when
-- the first module is compiled.  We need to write the results at every
-- compilation step since we don't know if the currently compiled module is the
-- last one or not.
--
updateTags :: FilePath
           -> Located (HsModule GhcPs)
           -> IO ()
updateTags tagsFile lmodule = do
  mTagsMap <- readIORef tagsIORef
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
  writeIORef tagsIORef (updatedTagsMap `seq` Just updatedTagsMap)

  -- update tags file
  withFile tagsFile WriteMode $ \fhandle ->
    BS.hPutBuilder fhandle
      $ foldMap formatVimTag
      $ sort
      $ concat
      $ Map.elems updatedTagsMap


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { tagSrcSpan, tagTag } =
    case tagSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName = TagName (fs_bs tagTag)
                   , tagFile = TagFile (fs_bs (srcSpanFile realSrcSpan))
                   , tagLine = srcSpanStartLine realSrcSpan
                   }


formatVimTag :: Tag -> Builder
formatVimTag Tag { tagName, tagFile, tagLine } =
       BS.byteString (getTagName tagName)
    <> BS.charUtf8 '\t'
    <> BS.byteString (getTagFile tagFile)
    <> BS.charUtf8 '\t'
    <> BS.intDec tagLine
    <> BS.charUtf8 '\n'
