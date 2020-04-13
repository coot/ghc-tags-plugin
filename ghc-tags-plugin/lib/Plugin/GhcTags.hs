{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Plugin.GhcTags ( plugin, Options (..) ) where

import           Control.Exception
import           Control.Monad.State.Strict
import           Data.ByteString (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
#if __GLASGOW_HASKELL__ < 808
import           Data.Functor (void, (<$))
#endif
import           Data.Functor.Identity (Identity (..))
import           Data.List (sortBy)
import           Data.Foldable (traverse_)
import           Data.Maybe (mapMaybe)
import           System.Directory
import           System.FilePath
import           System.FilePath.ByteString (RawFilePath)
import qualified System.FilePath.ByteString as FilePath
import           System.IO

#if !defined(mingw32_HOST_OS)
import           Foreign.C.Types (CInt (..))
import           Foreign.C.Error (throwErrnoIfMinus1_)
import           GHC.IO.FD (FD (..))
import           GHC.IO.Handle.FD (handleToFd)
#endif

import           Options.Applicative.Types (ParserFailure (..))

import qualified Pipes as Pipes
import           Pipes.Safe (SafeT)
import qualified Pipes.Safe as Pipes.Safe
import qualified Pipes.ByteString as Pipes.BS

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
ghcTagsPlugin options
              moduleSummary@ModSummary {ms_mod, ms_hspp_opts = dynFlags}
              hsParsedModule@HsParsedModule {hpm_module} =

    hsParsedModule <$
      case runOptionParser options of
        Success opts@Options { filePath = Identity tagsFile
                             , debug
                             } ->

           GhcPlugins.liftIO $ do
            let sourceFile = case splitFileName tagsFile of
                  (dir, name) -> dir </> "." ++ name
                lockFile = sourceFile ++ ".lock"

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
                    mbInSize <-
                      if debug
                        then Just <$> getFileSize tagsFile
                                      `catch` \(_ :: IOException) -> pure 0
                        else pure Nothing
                    updateTags opts moduleSummary hpm_module sourceFile
                    when debug $ do
                      let Just inSize = mbInSize
                      outSize <- getFileSize tagsFile
                      when (inSize > outSize)
                        $ throwIO (userError $ concat
                                    [ "tags file '"
                                    , tagsFile 
                                    , "' size shrinked: "
                                    , show inSize
                                    , "→"
                                    , show outSize
                                    ])

        Failure (ParserFailure f)  ->
          GhcPlugins.liftIO $
            putDocLn dynFlags
                     (messageDoc
                       OptionParserFailure
                       ms_mod
                       (show (case f "<ghc-tags-plugin>" of (h, _, _) -> h)
                         ++ " " ++ show options))

        CompletionInvoked {} -> error "ghc-tags-plugin: impossible happend"


data MessageType =
      ReadException
    | ParserException
    | WriteException
    | UnhandledException
    | OptionParserFailure
    | DebugMessage

instance Show MessageType where
    show ReadException       = "read error"
    show ParserException     = "tags parser error"
    show WriteException      = "write error"
    show UnhandledException  = "unhandled error"
    show OptionParserFailure = "plugin options parser error"
    show DebugMessage        = ""

-- | Extract tags from a module and update tags file
--
updateTags :: Options Identity
           -> ModSummary
           -> Located (HsModule GhcPs)
           -> FilePath
           -> IO ()
updateTags Options { etags, filePath = Identity tagsFile, debug }
           ModSummary {ms_mod, ms_location, ms_hspp_opts = dynFlags}
           lmodule sourceFile = do
  tagsFileExists <- doesFileExist tagsFile
  when tagsFileExists
    $ renameFile tagsFile sourceFile
  withFile tagsFile WriteMode  $ \writeHandle ->
    withFile sourceFile ReadWriteMode $ \readHandle -> do
      cwd <- BSC.pack <$> getCurrentDirectory
      -- absolute directory path of the tags file; we need canonical path
      -- (without ".." and ".") to make 'makeRelative' works.
      tagsDir <- BSC.pack <$> canonicalizePath (fst $ splitFileName tagsFile)

      case (etags, ml_hs_file ms_location) of

        --
        -- ctags
        --
        (False, Nothing)          -> pure ()
        (False, Just sourcePath) -> do

          let sourcePathBS = Text.encodeUtf8 (Text.pack sourcePath)
              -- text parser
              producer :: Pipes.Producer ByteString (SafeT IO) ()
              producer
                | tagsFileExists =
                    void (Pipes.BS.fromHandle readHandle)
                    `Pipes.Safe.catchP` \(e :: IOException) ->
                      Pipes.lift $ Pipes.liftIO $
                        -- don't re-throw; this would kill `ghc`, error
                        -- loudly and continue.
                        putDocLn dynFlags (messageDoc ReadException ms_mod (displayException e))
                | otherwise      = pure ()

              -- tags pipe
              pipe :: Pipes.Effect (StateT Int (StateT [CTag] (SafeT IO))) ()
              pipe =
                Pipes.for
                  (Pipes.hoist Pipes.lift $ Pipes.hoist Pipes.lift (tagParser (either (const Nothing) Just <$> CTag.parseTagLine) producer)
                    `Pipes.Safe.catchP` \(e :: IOException) ->
                      Pipes.lift $ Pipes.liftIO $
                        -- don't re-throw; this would kill `ghc`, error
                        -- loudly and continue.
                        putDocLn dynFlags $ messageDoc ParserException ms_mod (displayException e)
                  )
                  $
                  -- merge tags
                  (\tag -> do
                    modify' succ
                    Pipes.hoist Pipes.lift $
                        runCombineTagsPipe writeHandle
                          CTag.compareTags
                          CTag.formatTag
                          (fixFilePath cwd tagsDir sourcePathBS)
                          tag
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
          (parsedTags, tags') <- Pipes.Safe.runSafeT $ runStateT (execStateT (Pipes.runEffect pipe) 0) tags
          -- write the remaining tags'
          traverse_ (BSL.hPut writeHandle . BB.toLazyByteString . CTag.formatTag) tags'

          hFlush writeHandle
          -- hDataSync is necessary, otherwise next read will not get all the
          -- data, and the tags file will get truncated. Issue #37.
          hDataSync writeHandle

          when debug
            $ printMessageDoc dynFlags DebugMessage ms_mod
                (concat [ "parsed: "
                        , show parsedTags
                        , " found: "
                        , show (length tags)
                        , " left: "
                        , show (length tags')
                        ])

        --
        -- etags
        --
        (True, Nothing)         -> pure ()
        (True, Just sourcePath) ->
          try @IOException (BS.hGetContents readHandle)
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

                    let sourcePathBS = Text.encodeUtf8 (Text.pack sourcePath)

                        newTags  :: [ETag]
                        newTags =
                            (sortBy ETag.compareTags
                          . map ( ETag.withByteOffset ll
                                . fixTagFilePath cwd tagsDir
                                )
                          . mapMaybe (ghcTagToTag SingETag dynFlags)
                          . getGhcTags
                          $ lmodule)

                        tags' :: [ETag]
                        tags' = combineTags
                                  ETag.compareTags
                                  (fixFilePath cwd tagsDir sourcePathBS)
                                  newTags
                                  (sortBy ETag.compareTags tags)

                    when debug
                      $ printMessageDoc dynFlags DebugMessage ms_mod
                          (concat [ "parsed: "
                                  , show (length tags)
                                  , " found: "
                                  , show (length newTags)
                                  ])

                    BB.hPutBuilder writeHandle (ETag.formatETagsFile tags')


  where

    fixFilePath :: RawFilePath -- curent directory
                -> RawFilePath -- tags directory
                -> RawFilePath -> RawFilePath
    fixFilePath cwd tagsDir =
        FilePath.normalise
      . FilePath.makeRelative tagsDir
      . (cwd FilePath.</>)

    -- we are missing `Text` based `FilePath` library!
    fixTagFilePath :: RawFilePath -> RawFilePath -> Tag tk -> Tag tk
    fixTagFilePath cwd tagsDir tag@Tag { tagFilePath = TagFilePath fp } =
      tag { tagFilePath =
              TagFilePath
                (Text.decodeUtf8
                  (fixFilePath cwd tagsDir
                    (Text.encodeUtf8 fp)))
          }


--
-- Error Formatting
--

data MessageSeverity
      = Debug
      | Warning
      | Error

messageDoc :: MessageType -> Module -> String -> Out.SDoc
messageDoc errorType mod_ errorMessage =
    Out.blankLine
      $+$
        Out.coloured PprColour.colBold
          ((Out.text "GhcTagsPlugin: ")
            Out.<> (Out.coloured messageColour (Out.text $ show errorType)))
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
        Debug -> Out.blankLine
  where
    severity = case errorType of
      ReadException       -> Error
      ParserException     -> Error
      WriteException      -> Error
      UnhandledException  -> Error
      OptionParserFailure -> Warning
      DebugMessage        -> Debug

    messageColour = case severity of
      Error   -> PprColour.colRedFg
      Warning -> PprColour.colBlueFg
      Debug   -> PprColour.colCyanFg


putDocLn :: DynFlags -> Out.SDoc -> IO ()
putDocLn dynFlags sdoc =
    putStrLn $
      Out.renderWithStyle
        dynFlags
        sdoc
        (Out.setStyleColoured True $ Out.defaultErrStyle dynFlags)


printMessageDoc :: DynFlags -> MessageType -> Module -> String -> IO ()
printMessageDoc dynFlags = (fmap . fmap . fmap) (putDocLn dynFlags) messageDoc

--
-- Syscalls
--

#if !defined(mingw32_HOST_OS)
hDataSync ::  Handle -> IO ()
hDataSync h = do
    FD { fdFD } <- handleToFd h
    throwErrnoIfMinus1_ "ghc-tags-plugin" (c_fdatasync fdFD)

foreign import ccall safe "fdatasync"
    c_fdatasync :: CInt -> IO CInt 
#else
hDataSync :: Handle -> IO ()
hDataSync _ = pure ()
#endif
