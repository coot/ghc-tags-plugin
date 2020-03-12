{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.GhcTags ( plugin ) where

import           Control.Exception
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS
#if __GLASGOW_HASKELL__ < 808
import           Data.Functor ((<$))
#endif
import           Data.List (sortBy)
import           Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as Text
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error  (tryIOError)
import           System.FileLock  ( SharedExclusive (..)
                                  , withFileLock)

import           GhcPlugins ( CommandLineOption
                            , Hsc
                            , HsParsedModule (..)
                            , Located
                            , ModSummary (..)
                            , Plugin (..)
                            )
import qualified GhcPlugins
import           HsExtension (GhcPs)
import           HsSyn (HsModule (..))
import qualified Outputable as Out
import qualified PprColour

import           Plugin.GhcTags.Generate
import           Plugin.GhcTags.Tag
import qualified Plugin.GhcTags.Vim as Vim


-- | The GhcTags plugin.  It will run for every compiled module and have access
-- to parsed syntax tree.  It will inspect it and:
--
-- * update a global mutable state variable, which stores a tag map.
--   It is shared across modules compiled in the same `ghc` run.
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
      parsedResultAction = ghcTagsPlugin,
      pluginRecompile    = GhcPlugins.purePlugin
   }


-- | IOExcption wrapper; it is useful for the user to know that it's the plugin
-- not `ghc` that thrown the error.
--
data GhcTagsPluginException =
    GhcTagsPluginIOExceptino IOException
    deriving Show

instance Exception GhcTagsPluginException


-- | The plugin does not change the 'HsParedModule', it only runs side effects.
--
ghcTagsPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
ghcTagsPlugin options moduleSummary hsParsedModule@HsParsedModule {hpm_module} =
    hsParsedModule <$ GhcPlugins.liftIO (updateTags moduleSummary tagsFile hpm_module)
  where
    tagsFile :: FilePath
    tagsFile = case options of
      []    -> "tags"
      a : _ -> a


-- | Extract tags from a module and update tags file
--
updateTags :: ModSummary
           -> FilePath
           -> Located (HsModule GhcPs)
           -> IO ()
updateTags ModSummary {ms_mod, ms_hspp_opts = dynFlags} tagsFile lmodule =
    -- wrap 'IOException's
    handle (throwIO . GhcTagsPluginIOExceptino) $

      -- Take advisory exclusive lock (a BSD lock using `flock`) on the tags
      -- file.  This is needed when `cabal` compiles in parallel.
      withFileLock tagsFile Exclusive $ \_ -> do
        -- read and parse 
        a <- doesFileExist tagsFile
        tags <-
          if a
            then do
              res <- tryIOError $
                      Text.decodeUtf8 <$> BS.readFile tagsFile
                  >>= Vim.parseTagsFile
              case res of
                -- reading failed
                Left err -> do
                  putDocLn (errorDoc $ displayException err)
                  pure []

                -- parsing failed
                Right (Left err) -> do
                  -- shout and continue
                  putDocLn (errorDoc err)
                  pure []

                Right (Right tags) ->
                  pure tags

            -- no such file
            else pure []

        cwd <- getCurrentDirectory
        -- absolute directory path of the tags file; we need canonical path
        -- (without ".." and ".") to make 'makeRelative' works.
        tagsDir <- canonicalizePath (fst $ splitFileName tagsFile)

        let tags' :: [Tag]
            tags' =
                ( map (fixFileName cwd tagsDir)
                                       -- fix file names
                . sortBy compareTags   -- sort
                . mapMaybe ghcTagToTag -- translate 'GhcTag' to 'Tag'
                . getGhcTags           -- generate 'GhcTag's
                $ lmodule)
                `combineTags`
                tags
                
        -- update tags file
        withFile tagsFile WriteMode $ \h ->
          BS.hPutBuilder h (Vim.formatTagsFile tags')

  where

    fixFileName :: FilePath -> FilePath -> Tag -> Tag
    fixFileName cwd tagsDir tag@Tag { tagFile = TagFile path } =
      tag { tagFile = TagFile (makeRelative tagsDir (cwd </> path)) }

    errorDoc :: String -> Out.SDoc
    errorDoc errorMessage =
      Out.coloured PprColour.colBold
        $ Out.blankLine
            Out.$+$
              ((Out.text "GhcTagsPlugin: ")
                Out.<> (Out.coloured PprColour.colRedFg (Out.text "error:")))
            Out.$$
              (Out.nest 4 $ Out.ppr ms_mod)
            Out.$$
              (Out.nest 8 $ Out.coloured PprColour.colRedFg (Out.text errorMessage))
            Out.$+$
            Out.blankLine

    putDocLn :: Out.SDoc -> IO ()
    putDocLn sdoc =
        putStrLn $
          Out.renderWithStyle
            dynFlags
            sdoc
            (Out.setStyleColoured True $ Out.defaultErrStyle dynFlags)
