{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Plugin.GhcTags ( plugin, Options (..) ) where

import           Control.Exception
import           Control.Monad.State.Strict
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Builder as BB
#if __GLASGOW_HASKELL__ < 808
import           Data.Functor (void, (<$))
#endif
import           Data.Functor.Identity (Identity (..))
import           Data.List (sortBy)
import           Data.Foldable (traverse_)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           System.Directory
import           System.FilePath
import           System.IO

import           Options.Applicative.Types (ParserFailure (..))

import           Pipes ((~>))
import qualified Pipes as Pipes
import           Pipes.Safe (SafeT)
import qualified Pipes.Safe as Pipes.Safe
import qualified Pipes.ByteString as Pipes.BS
import qualified Pipes.Text.Encoding as Pipes.Text

import           GhcPlugins ( CommandLineOption
                            , DynFlags
                            , Hsc
                            , HsParsedModule (..)
                            , Located
                            , Module
                            , ModSummary (..)
                            , ModLocation (..)
                            , Plugin (..)
                            )
import qualified GhcPlugins
import           HsExtension (GhcPs)
import           HsSyn (HsModule (..))
import           Outputable (($+$), ($$))
import qualified Outputable as Out
import qualified PprColour

import           GhcTags.Ghc
import           GhcTags.Tag
import           GhcTags.Stream
import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag

import           Plugin.GhcTags.Options
import           Plugin.GhcTags.FileLock
import qualified Plugin.GhcTags.CTag as CTag


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
--  * /top level terms/
--  * /data types/
--  * /record fields/
--  * /type synonyms/
--  * /type classes/
--  * /type class members/
--  * /type class instances/
--  * /type families/                           /(standalone and associated)/
--  * /type family instances/                   /(standalone and associated)/
--  * /data type families/                      /(standalone and associated)/
--  * /data type families instances/            /(standalone and associated)/
--  * /data type family instances constructors/ /(standalone and associated)/
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
    hsParsedModule <$
      case runOptionParser options of
        Success opts -> GhcPlugins.liftIO (updateTags opts moduleSummary hpm_module)

        Failure (ParserFailure f)  -> GhcPlugins.liftIO $
          putDocLn (ms_hspp_opts moduleSummary)
                   (messageDoc
                     OptionParserFailure
                     (ms_mod moduleSummary)
                     (show $ case f "<ghc-tags-plugin>" of (h, _, _) -> h))

        CompletionInvoked {} -> error "ghc-tags-plugin: impossible happend"


data ExceptionType =
      ReadException
    | ParserException
    | WriteException
    | UnhandledException
    | OptionParserFailure

instance Show ExceptionType where
    show ReadException       = "read error"
    show ParserException     = "tags parser error"
    show WriteException      = "write error"
    show UnhandledException  = "unhandled error"
    show OptionParserFailure = "plugin options parser error"

-- | Extract tags from a module and update tags file
--
updateTags :: Options Identity
           -> ModSummary
           -> Located (HsModule GhcPs)
           -> IO ()
updateTags Options { etags, filePath = Identity tagsFile }
           ModSummary {ms_mod, ms_location, ms_hspp_opts = dynFlags}
           lmodule =
    -- wrap 'IOException's
    handle (\ioerr -> do
           putDocLn dynFlags (messageDoc UnhandledException ms_mod (displayException ioerr))
           throwIO $ GhcTagsPluginIOExceptino ioerr) $
     flip finally (void $ try @IOException $ removeFile sourceFile) $
        -- Take advisory exclusive lock (a BSD lock using `flock`) on the tags
        -- file.  This is needed when `cabal` compiles in parallel.
        -- We take the lock on the copy, otherwise the lock would be removed when
        -- we move the file.
        withFileLock lockFile ExclusiveLock WriteMode $ \_ -> do
          tagsFileExists <- doesFileExist tagsFile
          when tagsFileExists
            $ renameFile tagsFile sourceFile
          withFile tagsFile WriteMode  $ \writeHandle ->
            withFile sourceFile ReadWriteMode $ \readHandle -> do
              cwd <- getCurrentDirectory
              -- absolute directory path of the tags file; we need canonical path
              -- (without ".." and ".") to make 'makeRelative' works.
              tagsDir <- canonicalizePath (fst $ splitFileName tagsFile)

              case (etags, ml_hs_file ms_location) of

                --
                -- ctags
                --
                (False, Nothing)          -> pure ()
                (False, Just sourcePath) -> do

                  let -- text parser
                      producer :: Pipes.Producer Text (SafeT IO) ()
                      producer
                        | tagsFileExists =
                            void (Pipes.Text.decodeUtf8
                                   (Pipes.BS.fromHandle readHandle))
                            `Pipes.Safe.catchP` \(e :: IOException) ->
                              Pipes.lift $ Pipes.liftIO $
                                -- don't re-throw; this would kill `ghc`, error
                                -- loudly and continue.
                                putDocLn dynFlags (messageDoc ReadException ms_mod (displayException e))
                        | otherwise      = pure ()

                      -- gags pipe
                      pipe :: Pipes.Effect (StateT [CTag] (SafeT IO)) ()
                      pipe =
                        Pipes.for
                          (Pipes.hoist Pipes.lift (tagParser (either (const Nothing) Just <$> CTag.parseTagLine) producer)
                            `Pipes.Safe.catchP` \(e :: IOException) ->
                              Pipes.lift $ Pipes.liftIO $
                                -- don't re-throw; this would kill `ghc`, error
                                -- loudly and continue.
                                putDocLn dynFlags $ messageDoc ParserException ms_mod (displayException e)
                          )
                          $
                          -- normalise paths
                          (\tag -> Pipes.yield tag { tagFilePath = normalise (tagFilePath tag) })
                          ~>
                          -- merge tags
                          (\tag ->
                            runCombineTagsPipe writeHandle CTag.compareTags CTag.formatTag  (fixFilePath cwd tagsDir sourcePath) tag
                              `Pipes.Safe.catchP` \(e :: IOException) ->
                                Pipes.lift $ Pipes.liftIO $
                                  -- don't re-throw; this would kill `ghc`, error
                                  -- loudly and continue.
                                  putDocLn dynFlags $ messageDoc WriteException ms_mod (displayException e)
                          )

                  let tags :: [CTag]
                      tags = map (fixTagFilePath cwd tagsDir)
                                                  -- fix file names
                           . sortBy compareTags   -- sort
                           . mapMaybe (ghcTagToTag SingCTag dynFlags)
                                                  -- translate 'GhcTag' to 'Tag'
                           . getGhcTags           -- generate 'GhcTag's
                           $ lmodule

                  -- Write header
                  BSL.hPut writeHandle (BB.toLazyByteString (foldMap CTag.formatHeader CTag.headers))
                  -- update tags file / run 'pipe'
                  tags' <- Pipes.Safe.runSafeT $ execStateT ((Pipes.runEffect pipe)) tags
                  -- write the remaining tags'
                  traverse_ (BSL.hPut writeHandle . BB.toLazyByteString . CTag.formatTag) tags'

                --
                -- etags
                --
                (True, Nothing)         -> pure ()
                (True, Just sourcePath) ->
                  try @IOException (Text.decodeUtf8 <$> BS.hGetContents readHandle)
                    >>= \case
                      Left err ->
                        putDocLn dynFlags $ messageDoc ReadException ms_mod (displayException err)

                      Right txt -> do
                        pres <- try @IOException $ ETag.parseTagsFile txt
                        case pres of
                          Left err   -> 
                            putDocLn dynFlags $ messageDoc ParserException ms_mod (displayException err)

                          Right (Left err) ->
                            printMessageDoc dynFlags ParserException ms_mod err

                          Right (Right tags) -> do

                            -- read the source file to calculate byteoffsets
                            ll <- map (succ . BS.length) . BSC.lines <$> BS.readFile sourcePath

                            let tags' :: [ETag]
                                tags' = combineTags
                                          ETag.compareTags
                                          (fixFilePath cwd tagsDir sourcePath)
                                          (sortBy ETag.compareTags
                                            . map ( ETag.withByteOffset ll
                                                  . fixTagFilePath cwd tagsDir
                                                  )
                                            . mapMaybe (ghcTagToTag SingETag dynFlags)
                                            . getGhcTags
                                            $ lmodule)
                                          (sortBy ETag.compareTags tags)

                            BB.hPutBuilder writeHandle (ETag.formatETagsFile tags')


  where

    sourceFile = case splitFileName tagsFile of
      (dir, name) -> dir </> "." ++ name
    lockFile = sourceFile ++ ".lock"

    fixFilePath :: FilePath -- curent directory
                -> FilePath -- tags directory
                -> FilePath -> FilePath
    fixFilePath cwd tagsDir = normalise . makeRelative tagsDir . (cwd </>)

    fixTagFilePath :: FilePath -> FilePath -> Tag tk -> Tag tk
    fixTagFilePath cwd tagsDir tag@Tag { tagFilePath } =
      tag { tagFilePath = fixFilePath cwd tagsDir tagFilePath  }


--
-- Error Formatting
--

data MessageSeverity
      = Warning
      | Error

messageDoc :: ExceptionType -> Module -> String -> Out.SDoc
messageDoc errorType mod_ errorMessage =
    Out.blankLine
      $+$
        Out.coloured PprColour.colBold
          ((Out.text "GhcTagsPlugin: ")
            Out.<> (Out.coloured messageColour (Out.text $ show errorType ++ ":")))
      $$
        Out.coloured PprColour.colBold (Out.nest 4 $ Out.ppr mod_)
      $$
        (Out.nest 8 $ Out.coloured messageColour (Out.text errorMessage))
      $+$
        Out.blankLine
      $+$ case severity of
        Error ->
          Out.coloured PprColour.colBold (Out.text "Please report this bug to: ")
            Out.<> Out.text "https://github.com/coot/ghc-tags-plugin/issues"
          $+$ Out.blankLine
        Warning -> Out.blankLine
  where
    severity = case errorType of
      ReadException       -> Error
      ParserException     -> Error
      WriteException      -> Error
      UnhandledException  -> Error
      OptionParserFailure -> Warning

    messageColour = case severity of
      Error   -> PprColour.colRedFg
      Warning -> PprColour.colBlueFg


putDocLn :: DynFlags -> Out.SDoc -> IO ()
putDocLn dynFlags sdoc =
    putStrLn $
      Out.renderWithStyle
        dynFlags
        sdoc
        (Out.setStyleColoured True $ Out.defaultErrStyle dynFlags)


printMessageDoc :: DynFlags -> ExceptionType -> Module -> String -> IO ()
printMessageDoc dynFlags = (fmap . fmap . fmap) (putDocLn dynFlags) messageDoc
