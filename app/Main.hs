{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad (unless)
import           Data.Traversable
import           Data.Either
import           System.Environment
import           System.IO

import qualified Data.ByteString.Builder as BS

import           Lexer        ( ParseResult (..) )

import           Language.Haskell.GHC.Tags

main :: IO ()
main = do
  files <- getArgs
  (mtags :: [Either String [GhcTag]]) <-
    for files $ \file ->
      parseGhcModuleIO file >>= \case
        PFailed {}     -> return $ Left file
        POk _ hsModule -> return $ Right $ generateTagsForModule hsModule

  let 
      tags :: GhcTags
      (failed, tags) = concat <$> partitionEithers mtags

  unless (null failed)
    $ putStrLn $ "Failed to parse: " ++ show failed

  putStrLn $ "found " ++ show (length tags) ++ " tags"

  withFile "tags" WriteMode $ \handle ->
    BS.hPutBuilder handle (foldMap formatGhcTagVim tags)
