{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Monad ( filterM )

import           Data.Char ( toLower )
import           Data.Traversable
import           Data.Maybe
-- import           Data.Either
import           Data.Foldable ( traverse_ )
import           Data.Function ( on )
import           Data.Traversable ( traverse )
import           Data.List ( intersperse
                           , sortBy
                           )
import           System.Environment
import           System.Directory ( doesFileExist
                                  , getAccessTime
                                  , getModificationTime
                                  )
import           System.IO

import           System.FilePath ((</>))

import           Exception (gbracket)

import qualified Data.ByteString.Builder as BS

import qualified GHC.Paths

import           GHC.IO.Handle.Types
                              ( nativeNewlineMode
                              )
import           GHC.IO.Handle.Internals
                              ( mkFileHandle )
import qualified GHC.IO.FD as FD ( stderr )

-- Cabal imports
import qualified Distribution.Compiler                           ( CompilerInfo (..) )
import qualified Distribution.Compiler                  as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal ( readGenericPackageDescription )
import qualified Distribution.Simple                    as Cabal
import qualified Distribution.Simple.Configure          as Cabal
import qualified Distribution.Simple.Flag               as Cabal
import qualified Distribution.Simple.InstallDirs        as Cabal
import qualified Distribution.Simple.Program            as Cabal ( defaultProgramDb )
import qualified Distribution.Simple.Setup              as Cabal
import qualified Distribution.Simple.Setup              as Cabal ( defaultConfigFlags )
import qualified Distribution.Simple.UserHooks          as Cabal
import qualified Distribution.System                    as Cabal
import qualified Distribution.Text                      as Cabal ( display )
import qualified Distribution.Types.GenericPackageDescription  as Cabal
import qualified Distribution.Types.HookedBuildInfo     as Cabal ( emptyHookedBuildInfo )
import qualified Distribution.Types.LocalBuildInfo      as Cabal ( LocalBuildInfo (..) )
import qualified Distribution.Types.PackageDescription  as Cabal
import qualified Distribution.Types.PackageId           as Cabal
import qualified Distribution.Types.Version             as Cabal
import qualified Distribution.Verbosity                 as Cabal ( normal )

-- GHC imports
import           DriverPipeline
                              ( preprocess )
import           DriverPhases ( Phase (Cpp)
                              , HscSource (HsSrcFile)
                              )
import           DynFlags     ( DynFlags (..)
                              , Settings (..)
                              , parseDynamicFilePragma
                              )
import           GHC          ( Ghc
                              , runGhc
                              , setSessionDynFlags
                              )
import           GhcMonad     ( GhcMonad
                              , getSession
                              , getSessionDynFlags
                              , modifySession
                              , liftIO
                              )
import           HeaderInfo   ( getOptionsFromFile )
import           HsSyn        ( GhcPs
                              , HsModule
                              )
import           HscTypes     ( HscEnv (..) )
import           Lexer        ( ParseResult (..) )
import           Outputable   ( defaultUserStyle
                              , printSDocLn )
import           Pretty       ( Mode (..) )
import           StringBuffer ( hGetStringBuffer )
import           SrcLoc       ( Located )

import           Language.Haskell.GHC.Tags

data CppFlag =
      CppDefined String
    | CppInclude String
    | CppIncludeHeader String

getCppFlag :: CppFlag -> String
getCppFlag (CppDefined s)       = "-D"       ++ s
getCppFlag (CppInclude s)       = "-I"       ++ s
getCppFlag (CppIncludeHeader s) = "-include" ++ s

runGhcPreprocessor :: GhcMonad m => [CppFlag] -> FilePath -> m FilePath
runGhcPreprocessor cppFlags filePath = do
    modifySession injectCppFlags
    hscEnv <- getSession
    (_, tempFile) <- liftIO $ preprocess hscEnv (filePath, Just (Cpp HsSrcFile))
    return tempFile
  where
    -- todo: use lenses
    injectCppFlags :: HscEnv -> HscEnv
    injectCppFlags
      hsc@HscEnv { hsc_dflags
        = dynFlags@DynFlags { settings
            = settings@Settings { sOpt_P }
          }
      } =
      hsc { hsc_dflags
              = dynFlags { settings
                = settings { sOpt_P
                  = sOpt_P ++ getCppFlag `map` cppFlags
                }
              }
          }


-- | Run a 'Ghc' computation in the context of 'DynFlags' read from a file.
--
withFileDynFlags :: FilePath
                 -> Ghc a
                 -- ^ Ghc action executed with 'DynFlags' ammended by the local file
                 -> Ghc a
withFileDynFlags filePath k =
    gbracket
      getSessionDynFlags
      setSessionDynFlags
      $ \dynFlags -> do
        opts <- liftIO $ getOptionsFromFile dynFlags filePath
        (dynFlags', _, _)
          <- parseDynamicFilePragma dynFlags opts
        _ <- setSessionDynFlags dynFlags'
        k


-- | Read a file, add options defined in its pragmas, parse it and return the
-- result.
--
ghcTagsForModule :: FilePath -> Ghc (Maybe (Located (HsModule GhcPs)))
ghcTagsForModule filePath = do
    liftIO $ putStrLn ("parsing " ++ filePath)
    withFileDynFlags filePath $ do
      processedFilePath <- runGhcPreprocessor [] filePath
      source <- liftIO $ hGetStringBuffer processedFilePath
      parseModuleGhc filePath source >>= \case
        POk _ a -> return $ Just a

        PFailed f srcSpan msgDoc -> do
          curDynFlags <- getSessionDynFlags
          liftIO $ do
            let (warningMessages, errorMessages) = f curDynFlags
            traverse_ (putStrLn . show) warningMessages
            traverse_ (putStrLn . show) errorMessages
            fhandle <- mkFileHandle
                  FD.stderr
                  "/dev/null"
                  WriteMode
                  Nothing -- todo: encoding
                  nativeNewlineMode
            putStrLn (show srcSpan)
            printSDocLn PageMode curDynFlags
                        fhandle
                        (defaultUserStyle curDynFlags)
                        msgDoc
            return Nothing

data Optimization = NoOpt | Opt | NormalOpt
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  cabalFilePaths <- getArgs
  parseResults <-
    for cabalFilePaths $ \cabalFilePath ->
      -- todo: add error handler, we can continue even if one the modules does not parse
      runGhc (Just GHC.Paths.libdir) $ do
        liftIO $ do
          Cabal.GenericPackageDescription { Cabal.packageDescription = packageDescription } <- Cabal.readGenericPackageDescription Cabal.normal cabalFilePath

          -- this is a hack to construct the path to read 'setup-config'
          let packageId = Cabal.package packageDescription
              -- this is modeled after 'Distribution.Client.DistDirLayout.defaultDistDirLayout'
              basePath = "dist-newstyle"
                </> "build"
                </> Cabal.display Cabal.buildPlatform
                -- compiler should be configurable or read using @ghc --numeric-version@
                </> Cabal.display (Cabal.CompilerId Cabal.GHC (Cabal.mkVersion [8, 6, 5]))
                </> Cabal.display packageId

          -- chose the most recently modified file
          paths <- filterM (doesFileExist . (</> "setup-config"))
                      [ basePath
                      , basePath </> "noopt"
                      , basePath </> "opt"
                      ]
          modTimes <- traverse (getAccessTime . (</> "setup-config")) paths
          case sortBy (on (flip compare) snd)
                      (zip paths modTimes) of

            []                   -> return ()
            (path, _) : _ -> do
              putStrLn path
              -- todo: can error
              lbi <- Cabal.getPersistBuildConfig path

              putStrLn $ "buildDir " ++ Cabal.buildDir lbi
              -- buildDir /home/coot/repos/haskell/ghctags/dist-newstyle/build/x86_64-linux/ghc-8.6.5/ghctags-0.1.0.0/noop
              --  buildDir </> "autogen" </> "cabal_macros.h"

              -- iterate over all components and run 'runGhcPreprocessor'
              -- we need to setup DynFlags which include the 'cabal_macros.h' file
              --
              -- each component might has its own "cabal_macros.h" file!
              -- "x" </> componentName
              --     </> "noopt"  -- can be "opt" or not at all
              --     </> "build"
              --     </> comonentName
              --     </> "autogen"
              --     </> "cabal_macros.h"
              return ()
        return Nothing

  let tags :: GhcTags
      tags = concat . map generateTagsForModule . catMaybes $ parseResults

  {-
  unless (null failed)
    $ traverse_ (\(file, msg) -> putStrLn $ "Failed to parse: " ++ file ++ "\n" ++ show msg)
                failed
  -}

  putStrLn $ "found " ++ show (length tags) ++ " tags"

  withFile "tags" WriteMode $ \fhandle ->
    BS.hPutBuilder fhandle (foldMap formatGhcTagVim tags)
