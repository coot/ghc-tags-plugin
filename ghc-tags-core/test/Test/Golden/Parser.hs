{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Parser (tests) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad ((>=>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import           System.IO
import           System.FilePath

import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden


tests :: FilePath -> TestTree
tests goldenTestDir =
    testGroup "Golden.Parser" $
      [ testGroup "CTag"
          [ let input  = goldenTestDir </> "test.tags"
                output = goldenTestDir </> "test.tags.out"
            in goldenVsFile
                "test tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "vim.tags"
                output = goldenTestDir </> "vim.tags.out"
            in goldenVsFile
                "vim tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "typed-protocols.tags"
                output = goldenTestDir </> "typed-protocols.tags.out"
            in goldenVsFile
                "typed-protocols tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "io-sim-classes.tags"
                output = goldenTestDir </> "io-sim-classes.tags.out"
            in goldenVsFile
                "io-sim-classes tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)
         
          , let input  = goldenTestDir </> "ouroboros-network.tags"
                output = goldenTestDir </> "ouroboros-network.tags.out"
            in goldenVsFile 
                "ouroboros-network tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "ouroboros-consensus.tags"
                output = goldenTestDir </> "ouroboros-consensus.tags.out"
            in goldenVsFile
                "ouroboros-consensus tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "ghc.tags"
                output = goldenTestDir </> "ghc.tags.out"
            in goldenVsFile
                "ghc tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)
          ]

      , testGroup "ETag"
          [ let input  = goldenTestDir </> "ouroboros-consensus.ETAGS"
                output = goldenTestDir </> "ouroboros-consensus.ETAGS.out"
            in goldenVsFile 
                "ouroboros-consensus TAGS"
                 input
                 output
                 (parseGoldenETagsFile input output)

          , let input  = goldenTestDir </> "vim.ETAGS"
                output = goldenTestDir </> "vim.ETAGS.out"
            in goldenVsFile 
                "vim tags"
                 input
                 output
                 (parseGoldenETagsFile input output)

          , let input  = goldenTestDir </> "ghc.ETAGS"
                output = goldenTestDir </> "ghc.ETAGS.out"
            in goldenVsFile 
                "ghc tags"
                 input
                 output
                 (parseGoldenETagsFile input output)
          ]
      ]


parseGoldenCTagsFile
    :: FilePath -- input file
    -> FilePath -- output file
    -> IO ()
parseGoldenCTagsFile input output = do
    res <- withBinaryFile input ReadMode
      (BS.hGetContents >=> CTag.parseTagsFile)
    case res of
      Left  err  -> throwIO (userError err)
      Right tags ->
        withBinaryFile output WriteMode
          $ flip BS.hPutBuilder (foldMap (CTag.formatHeader ||| CTag.formatTag) tags)


parseGoldenETagsFile
    :: FilePath -- ^ input file
    -> FilePath -- ^ output file
    -> IO ()
parseGoldenETagsFile input output = do
    res <- withBinaryFile input ReadMode
      (BS.hGetContents >=> ETag.parseTagsFile)
    case res of
      Left  err  -> throwIO (userError err)
      Right tags ->
        withBinaryFile output WriteMode
          $ flip BS.hPutBuilder (ETag.formatETagsFile tags)
