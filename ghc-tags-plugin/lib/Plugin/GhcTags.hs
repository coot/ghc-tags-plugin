{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Plugin.GhcTags ( plugin, Options (..) ) where

import           Control.Exception
import           Control.Monad.State.Strict
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
#if __GLASGOW_HASKELL__ >= 810
import           Data.Either (partitionEithers)
#endif
import           Data.Maybe (mapMaybe)
import           System.Directory
import           System.FilePath
import           System.FilePath.ByteString (RawFilePath)
import qualified System.FilePath.ByteString as FilePath
import           System.IO

import           Options.Applicative.Types (ParserFailure (..))

#if __GLASGOW_HASKELL__ >= 900
import           GHC.Driver.Plugins
#else
import           GhcPlugins
#endif
                            ( CommandLineOption
                            , Plugin (..)
                            )
#if    __GLASGOW_HASKELL__ >= 900
import qualified GHC.Driver.Plugins as GhcPlugins
#if    __GLASGOW_HASKELL__ >= 902
import           GHC.Driver.Env   ( Hsc
                                  , HscEnv (..)
                                  )
import           GHC.Hs           (HsParsedModule (..))
import           GHC.Unit.Module.ModSummary
                                  (ModSummary (..))
import           GHC.Types.Meta   ( MetaHook
                                  , MetaRequest (..)
                                  , MetaResult
                                  , metaRequestAW
                                  , metaRequestD
                                  , metaRequestE
                                  , metaRequestP
                                  , metaRequestT
                                  )
#else
import           GHC.Driver.Types ( Hsc
                                  , HsParsedModule (..)
                                  , ModSummary (..)
                                  , MetaHook
                                  , MetaRequest (..)
                                  , MetaResult
                                  , metaRequestAW
                                  , metaRequestD
                                  , metaRequestE
                                  , metaRequestP
                                  , metaRequestT
                                  )
#endif
import           GHC.Driver.Hooks (Hooks (..))
import           GHC.Unit.Types   (Module)
import           GHC.Unit.Module.Location   (ModLocation (..))
import           GHC.Tc.Types (TcM)
import           GHC.Tc.Gen.Splice (defaultRunMeta)
import           GHC.Types.SrcLoc (Located)
import qualified GHC.Types.SrcLoc as GHC (SrcSpan (..), getLoc, srcSpanFile)
#else
import qualified GhcPlugins
import           GhcPlugins ( Hsc
                            , HsParsedModule (..)
                            , Located
                            , Module
                            , ModLocation (..)
                            , ModSummary (..)
#if __GLASGOW_HASKELL__ >= 810
                            , MetaHook
                            , MetaRequest (..)
                            , MetaResult
                            , metaRequestAW
                            , metaRequestD
                            , metaRequestE
                            , metaRequestP
                            , metaRequestT
#endif
                            )
import qualified SrcLoc as GHC (SrcSpan (..), getLoc, srcSpanFile)
#endif
#if   __GLASGOW_HASKELL__ >= 902
import           GHC.Driver.Session (DynFlags)
#elif __GLASGOW_HASKELL__ >= 900
import           GHC.Driver.Session (DynFlags (DynFlags, hooks))
#elif __GLASGOW_HASKELL__ >= 810
import           DynFlags (DynFlags (DynFlags, hooks))
#else
import           DynFlags (DynFlags)
#endif

#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Hs (GhcPs, GhcTc, HsModule (..), LHsDecl, LHsExpr)
#elif __GLASGOW_HASKELL__ >= 810
import           GHC.Hs (GhcPs, GhcTc, HsModule (..), LHsDecl, LHsExpr)
import           TcSplice
import           TcRnMonad
import           Hooks
#else
import           HsExtension (GhcPs)
import           HsSyn (HsModule (..))
#endif
#if __GLASGOW_HASKELL__ >= 900
import           GHC.Utils.Outputable (($+$), ($$))
import qualified GHC.Utils.Outputable as Out
import qualified GHC.Utils.Ppr.Colour as PprColour
#else
import           Outputable (($+$), ($$))
import qualified Outputable as Out
import qualified PprColour
#endif
#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Data.FastString (bytesFS)
#elif __GLASGOW_HASKELL__ >= 810
import           FastString          (bytesFS)
#else
import           FastString          (FastString (fs_bs))
#endif

import           GhcTags.Ghc
import           GhcTags.Tag
import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag

import           Plugin.GhcTags.Options
import           Plugin.GhcTags.FileLock


#if   __GLASGOW_HASKELL__ < 810
bytesFS :: FastString -> ByteString
bytesFS = fs_bs
#endif

#if   __GLASGOW_HASKELL__ >= 900
type GhcPsModule = HsModule
#else
type GhcPsModule = HsModule GhcPs
#endif


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
      parsedResultAction = ghcTagsParserPlugin,
#if   __GLASGOW_HASKELL__ >= 902
      driverPlugin       = ghcTagsDriverPlugin,
#elif __GLASGOW_HASKELL__ >= 810
      dynflagsPlugin     = ghcTagsDynflagsPlugin,
#endif
      pluginRecompile    = GhcPlugins.purePlugin
   }


-- | IOExcption wrapper; it is useful for the user to know that it's the plugin
-- not `ghc` that thrown the error.
--
data GhcTagsPluginException
    = GhcTagsParserPluginIOException IOException
    | GhcTagsDynFlagsPluginIOException IOException
    deriving Show

instance Exception GhcTagsPluginException


-- | The plugin does not change the 'HsParedModule', it only runs side effects.
--
ghcTagsParserPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
ghcTagsParserPlugin options
                    moduleSummary@ModSummary {ms_mod, ms_hspp_opts = dynFlags}
                    hsParsedModule@HsParsedModule {hpm_module} =

    hsParsedModule <$
      case runOptionParser options of
        Success opts@Options { filePath = Identity tagsFile
                             , debug
                             } ->

           liftIO $ do
            -- wrap 'IOException's
            handle (\ioerr -> do
                     putDocLn dynFlags
                              (messageDoc UnhandledException (Just ms_mod)
                                (displayException ioerr))
                     throwIO (GhcTagsParserPluginIOException ioerr)) $
                -- Take advisory exclusive lock (a BSD lock using `flock`) on the tags
                -- file.  This is needed when `cabal` compiles in parallel.
                -- We take the lock on the copy, otherwise the lock would be removed when
                -- we move the file.
                withFileLock debug tagsFile ExclusiveLock $ \_ -> do
                    mbInSize <-
                      if debug
                        then Just <$> getFileSize tagsFile
                                      `catch` \(_ :: IOException) -> pure 0
                        else pure Nothing
                    updateTags opts moduleSummary hpm_module
                    when debug $ do
                      let Just inSize = mbInSize
                      outSize <- getFileSize tagsFile
                      when (inSize > outSize)
                        $ liftIO
                        $ putDocLn dynFlags
                            (messageDoc SizeWarning
                                        (Just ms_mod)
                                        (concat [ show inSize
                                                , "→"
                                                , show outSize
                                                ]))

        Failure (ParserFailure f)  ->
          liftIO $
            putDocLn dynFlags
                     (messageDoc
                       OptionParserFailure
                       (Just ms_mod)
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
    | SizeWarning


instance Show MessageType where
    show ReadException       = "read error"
    show ParserException     = "tags parser error"
    show WriteException      = "write error"
    show UnhandledException  = "unhandled error"
    show OptionParserFailure = "plugin options parser error"
    show SizeWarning         = "tags file shrinked"
    show DebugMessage        = ""


-- | Extract tags from a module and update tags file
--
updateTags :: Options Identity
           -> ModSummary
           -> Located GhcPsModule
           -> IO ()
updateTags Options { etags, filePath = Identity tagsFile, debug }
           ModSummary {ms_mod, ms_location, ms_hspp_opts = dynFlags}
           lmodule = do
  tagsFileExists <- doesFileExist tagsFile

  mbInSize <-
    if debug
      then
        if tagsFileExists
          then Just <$> getFileSize tagsFile
                    `catch` \(_ :: IOException) -> pure 0
          else pure (Just 0)
      else pure Nothing

  !tagsContent <- if tagsFileExists
                    then BS.readFile tagsFile
                    else return mempty
  withFile tagsFile WriteMode $ \writeHandle -> do
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
              -- path of the combiled module; it is relative to the cabal file,
              -- not the project.
              modulePath =
                case GHC.getLoc lmodule of
#if __GLASGOW_HASKELL__ >= 900
                  GHC.RealSrcSpan rss _ ->
#else
                  GHC.RealSrcSpan rss ->
#endif
                      bytesFS
                    . GHC.srcSpanFile
                    $ rss
                  GHC.UnhelpfulSpan {} ->
                    fixFilePath cwd tagsDir sourcePathBS

          pres <- try @IOException $ CTag.parseTagsFile tagsContent
          case pres of
            Left err   ->
              putDocLn dynFlags $ messageDoc ParserException (Just ms_mod) (displayException err)

            Right (Left err) ->
              printMessageDoc dynFlags ParserException (Just ms_mod) err

            Right (Right !parsed) -> do
              let (headers, !parsedTags) = partitionEithers parsed 

                  tags :: [CTag]
                  tags = map (fixTagFilePath cwd tagsDir)
                                              -- fix file names
                       . filterAdjacentTags
                       . sortBy compareTags   -- sort
                       . mapMaybe (ghcTagToTag SingCTag dynFlags)
                                              -- translate 'GhcTag' to 'Tag'
                       . getGhcTags           -- generate 'GhcTag's
                       $ lmodule

                  combined :: [CTag]
                  combined = combineTags CTag.compareTags modulePath tags parsedTags

              BB.hPutBuilder writeHandle
                        (    foldMap CTag.formatHeader headers
                          <> foldMap CTag.formatTag combined
                        ) 

              when debug $ do
                outSize <- getFileSize tagsFile
                let Just inSize = mbInSize
                printMessageDoc dynFlags DebugMessage (Just ms_mod)
                  (concat [ "parsed: "
                          , show (length parsedTags)
                          , " found: "
                          , show (length tags)
                          , " in-size: "
                          , show inSize
                          , " out-size: "
                          , show outSize
                          ])

        --
        -- etags
        --
        (True, Nothing)         -> pure ()
        (True, Just sourcePath) -> do
                pres <- try @IOException $ ETag.parseTagsFile tagsContent
                case pres of
                  Left err   ->
                    putDocLn dynFlags $ messageDoc ParserException (Just ms_mod) (displayException err)

                  Right (Left err) ->
                    printMessageDoc dynFlags ParserException (Just ms_mod) err

                  Right (Right parsedTags) -> do
                    let sourcePathBS = Text.encodeUtf8 (Text.pack sourcePath)

                        tags :: [ETag]
                        tags = filterAdjacentTags
                             . sortBy ETag.compareTags
                             . map (fixTagFilePath cwd tagsDir)
                             . mapMaybe (ghcTagToTag SingETag dynFlags)
                             . getGhcTags
                             $ lmodule

                        combined :: [ETag]
                        combined = combineTags ETag.compareTags
                                     (fixFilePath cwd tagsDir sourcePathBS)
                                     tags
                                     (sortBy ETag.compareTags tags)

                    BB.hPutBuilder writeHandle (ETag.formatETagsFile combined)

                    when debug $ do
                      outSize <- getFileSize tagsFile
                      let Just inSize = mbInSize
                      printMessageDoc dynFlags DebugMessage (Just ms_mod)
                        (concat [ "parsed: "
                                , show (length parsedTags)
                                , " found: "
                                , show (length tags)
                                , " in-size: "
                                , show inSize
                                , " out-size: "
                                , show outSize
                                ])



-- | Filter adjacent tags.
--
filterAdjacentTags :: [Tag tk] -> [Tag tk]
filterAdjacentTags tags =
    foldr
      (\(mprev, c, mnext) acc ->
          case (mprev, mnext) of
            -- filter out terms preceded by a type signature
            (Just p, _)  | tagName p == tagName c
                         , TkTypeSignature <- tagKind p
                         , k <- tagKind c
                         , k == TkTerm
                        || k == TkFunction
                        ->     acc

            -- filter out type constructors followed by a data constructor
            (_, Just n)  | tagName c == tagName n
                         , TkTypeConstructor <- tagKind c
                         , k <- tagKind n
                         , k == TkDataConstructor
                        || k == TkGADTConstructor
                        ->     acc

            _           -> c : acc

      )
      []
      (zip3 tags' tags tags'')
  where
    -- previous
    tags' = case tags of
      [] -> []
      _  -> Nothing : map Just (init tags)

    -- next
    tags'' = case tags of
      [] -> []
      _  -> map Just (tail tags) ++ [Nothing]


#if __GLASGOW_HASKELL__ >= 810
--
-- Tags for Template-Haskell splices
--

#if __GLASGOW_HASKELL__ >= 902
ghcTagsDriverPlugin :: [CommandLineOption] -> HscEnv -> IO HscEnv
ghcTagsDriverPlugin opts env@HscEnv{ hsc_hooks } = do
    let hook = ghcTagsMetaHook opts (hsc_dflags env)
    return env { hsc_hooks = hsc_hooks { runMetaHook = Just hook } }
#else
ghcTagsDynflagsPlugin :: [CommandLineOption] -> DynFlags -> IO DynFlags
ghcTagsDynflagsPlugin options dynFlags@DynFlags { hooks } = do
    let hook = ghcTagsMetaHook options dynFlags
    return dynFlags { hooks = hooks { runMetaHook = Just hook } }

#endif

-- | DynFlags plugin which extract tags from TH splices.
--
ghcTagsMetaHook :: [CommandLineOption] -> DynFlags -> MetaHook TcM
ghcTagsMetaHook options dynFlags request expr =
    case runOptionParser options of
      Success Options { filePath = Identity tagsFile
                      , etags
                      , debug
                      } -> do

        withMetaD defaultRunMeta request expr $ \decls ->
          liftIO $
            handle (\ioerr -> do
                     putDocLn dynFlags
                             (messageDoc UnhandledException Nothing
                               (displayException ioerr))
                     throwIO (GhcTagsDynFlagsPluginIOException ioerr)) $
            withFileLock debug tagsFile ExclusiveLock $ \_ -> do
            cwd <- BSC.pack <$> getCurrentDirectory
            tagsDir <- BSC.pack <$> canonicalizePath (fst $ splitFileName tagsFile)
            tagsContent <- BSC.readFile tagsFile
            if etags
              then do
                pr <- ETag.parseTagsFile tagsContent
                case pr of
                  Left err ->
                    printMessageDoc dynFlags ParserException Nothing err

                  Right tags -> do
                    let tags' :: [ETag]
                        tags' = sortBy ETag.compareTags $
                                  tags
                                  ++
                                  (fmap (fixTagFilePath  cwd tagsDir)
                                  . ghcTagToTag SingETag dynFlags)
                                    `mapMaybe`
                                     hsDeclsToGhcTags Nothing decls
                    BSL.writeFile tagsFile (BB.toLazyByteString $ ETag.formatTagsFile tags')
              else do
                pr <- fmap partitionEithers <$> CTag.parseTagsFile tagsContent
                case pr of
                  Left err ->
                    printMessageDoc dynFlags ParserException Nothing err

                  Right (headers, tags) -> do
                    let tags' :: [Either CTag.Header CTag]
                        tags' = Left `map` headers
                             ++ Right `map`
                                sortBy CTag.compareTags
                                ( tags
                                  ++
                                  (fmap (fixTagFilePath  cwd tagsDir)
                                    . ghcTagToTag SingCTag dynFlags)
                                    `mapMaybe`
                                    hsDeclsToGhcTags Nothing decls
                                )
                    BSL.writeFile tagsFile (BB.toLazyByteString $ CTag.formatTagsFile tags')

      Failure (ParserFailure f)  ->
        withMetaD defaultRunMeta request expr $ \_ ->
        liftIO $
          putDocLn dynFlags
                   (messageDoc
                     OptionParserFailure
                     Nothing
                     (show (case f "<ghc-tags-plugin>" of (h, _, _) -> h)
                       ++ " " ++ show options))

      CompletionInvoked {} -> error "ghc-tags-plugin: impossible happend"

  where
    -- run the hook and call call the callback with new declarations
    withMetaD :: MetaHook TcM -> MetaRequest -> LHsExpr GhcTc
                    -> ([LHsDecl GhcPs] -> TcM a)
                    -> TcM (MetaResult)
    withMetaD h req e f = case req of
      MetaE  k -> k <$> metaRequestE h e
      MetaP  k -> k <$> metaRequestP h e
      MetaT  k -> k <$> metaRequestT h e
      MetaD  k -> do
        res <- metaRequestD h e
        k res <$ f res
      MetaAW k -> k <$> metaRequestAW h e
#endif


--
-- File path utils
--

fixFilePath :: RawFilePath
            -- ^ curent directory
            -> RawFilePath
            -- ^ tags file directory
            -> RawFilePath
            -- ^ tag's file path
            -> RawFilePath
fixFilePath cwd tagsDir =
    FilePath.normalise
  . FilePath.makeRelative tagsDir
  . (cwd FilePath.</>)


-- we are missing `Text` based `FilePath` library!
fixTagFilePath :: RawFilePath
               -- ^ current directory
               -> RawFilePath
               -- ^ tags file directory
               -> Tag tk -> Tag tk
fixTagFilePath cwd tagsDir tag@Tag { tagFilePath = TagFilePath fp } =
  tag { tagFilePath =
          TagFilePath
            (Text.decodeUtf8
              (fixFilePath cwd tagsDir
                (Text.encodeUtf8 fp)))
      }

--
-- Error Formattng
--

data MessageSeverity
      = Debug
      | Warning
      | Error

messageDoc :: MessageType -> Maybe Module -> String -> Out.SDoc
messageDoc errorType mb_mod errorMessage =
    Out.blankLine
      $+$
        Out.coloured PprColour.colBold
          ((Out.text "GhcTagsPlugin: ")
            Out.<> (Out.coloured messageColour (Out.text $ show errorType)))
      $$
        case mb_mod of
          Just mod_ ->
            Out.coloured PprColour.colBold (Out.nest 4 $ Out.ppr mod_)
          Nothing -> Out.empty
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
      SizeWarning         -> Warning
      DebugMessage        -> Debug

    messageColour = case severity of
      Error   -> PprColour.colRedFg
      Warning -> PprColour.colBlueFg
      Debug   -> PprColour.colCyanFg


putDocLn :: DynFlags -> Out.SDoc -> IO ()
#if   __GLASGOW_HASKELL__ >= 902
putDocLn _dynFlags sdoc =
#else
putDocLn  dynFlags sdoc =
#endif
    putStrLn $
#if   __GLASGOW_HASKELL__ >= 902
      Out.renderWithContext
        Out.defaultSDocContext { Out.sdocStyle = Out.mkErrStyle Out.neverQualify }
        sdoc
#elif __GLASGOW_HASKELL__ >= 900
      Out.renderWithStyle
        (Out.initSDocContext
          dynFlags
          (Out.setStyleColoured False
            $ Out.mkErrStyle Out.neverQualify))
        sdoc
#else
      Out.renderWithStyle
        dynFlags
        sdoc
        (Out.setStyleColoured True $ Out.defaultErrStyle dynFlags)
#endif


printMessageDoc :: DynFlags -> MessageType -> Maybe Module -> String -> IO ()
printMessageDoc dynFlags = (fmap . fmap . fmap) (putDocLn dynFlags) messageDoc
