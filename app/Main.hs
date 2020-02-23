{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Traversable
import           Data.Maybe
-- import           Data.Either
import           Data.Foldable (traverse_)
import           System.Environment
import           System.IO

import qualified Data.ByteString.Builder as BS

import qualified GHC.Paths

import           GHC.IO.Handle.Types
                              ( nativeNewlineMode
                              )
import           GHC.IO.Handle.Internals
                              ( mkFileHandle )
import qualified GHC.IO.FD as FD ( stderr )

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
import           GHC          ( runGhc
                              , setSessionDynFlags
                              )
import           GhcMonad     ( GhcMonad
                              , getSession
                              , getSessionDynFlags
                              , modifySession
                              , liftIO
                              )
import           HeaderInfo   ( getOptionsFromFile )
import           HscTypes     ( HscEnv (..) )
import           Lexer        ( ParseResult (..) )
import           Outputable   ( defaultUserStyle
                              , printSDocLn )
import           Pretty       ( Mode (..) )
import           StringBuffer ( hGetStringBuffer )

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

main :: IO ()
main = do
  files <- getArgs
  parseResults <-
    for files $ \filePath ->
      -- todo: add error handler, we can continue even if one the modules does not parse
      runGhc (Just GHC.Paths.libdir) $ do
        liftIO $ putStrLn ("parsing " ++ filePath)
        dynFlags <- getSessionDynFlags
        -- these are flags from a file, we also need flags from the
        -- corresponding cabal file and `cabal_macros.h` and these might defer
        -- per cabal project (generated macros depend on depending packages).
        opts <- liftIO $ getOptionsFromFile dynFlags filePath
        (dynFlags', _, _)
          <- parseDynamicFilePragma dynFlags opts
        _ <- setSessionDynFlags dynFlags'
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
              handle <- mkFileHandle
                    FD.stderr
                    "/dev/null"
                    WriteMode
                    Nothing -- todo: encoding
                    nativeNewlineMode
              putStrLn (show srcSpan)
              printSDocLn PageMode dynFlags
                          handle
                          (defaultUserStyle dynFlags)
                          msgDoc
              return Nothing

  let tags :: GhcTags
      tags = concat . map generateTagsForModule . catMaybes $ parseResults

  {-
  unless (null failed)
    $ traverse_ (\(file, msg) -> putStrLn $ "Failed to parse: " ++ file ++ "\n" ++ show msg)
                failed
  -}

  putStrLn $ "found " ++ show (length tags) ++ " tags"

  withFile "tags" WriteMode $ \handle ->
    BS.hPutBuilder handle (foldMap formatGhcTagVim tags)
