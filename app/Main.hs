{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad (unless)
import           Data.Traversable
import           Data.Maybe
import           Data.Either
import           Data.Foldable (traverse_)
import           System.Environment
import           System.IO

import qualified Data.ByteString.Builder as BS

import           Lexer        ( ParseResult (..) )

import qualified GHC.Paths

import           GHC.IO.Handle.Types
                              ( nativeNewlineMode
                              )
import           GHC.IO.Handle.Internals
                              ( mkFileHandle )
import qualified GHC.IO.FD as FD ( stderr )

-- GHC imports
import           GHC          ( runGhc )
import           GhcMonad     ( Ghc
                              , reifyGhc
                              , getSessionDynFlags
                              )
import           Lexer        ( P (unP)
                              , ParseResult (..)
                              , mkPState
                              )
import           Outputable   ( defaultUserStyle
                              , printSDocLn )
import           Pretty       ( Mode (..) )

import           Language.Haskell.GHC.Tags

main :: IO ()
main = do
  files <- getArgs
  parseResults <-
    runGhc (Just GHC.Paths.libdir) $ do
      for files $ \file -> do
        parseModuleGhc file >>= \case
          POk _ a -> return $ Just a
          PFailed f srcSpan msgDoc -> do
            dynFlags <- getSessionDynFlags
            reifyGhc $ \_ -> do
              let (warningMessages, errorMessages) = f dynFlags
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
