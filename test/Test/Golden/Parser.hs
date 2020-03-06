module Test.Golden.Parser (tests) where

import           Control.Exception
import           Control.Monad ((>=>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           System.IO

import qualified Plugin.GhcTags.Vim as Vim

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced


tests :: TestTree
tests = testGroup "Golden.Parser"
  [ let input  = "test/golden/test.tags"
        output = "test/golden/test.tags.out"
    in goldenVsFile 
        "test tags"
         input
         output
         (parseGoldenFile input output)

  , let input  = "test/golden/typed-protocols.tags"
        output = "test/golden/typed-protocols.tags.out"
    in goldenVsFile 
        "typed-protocols tags"
         input
         output
         (parseGoldenFile input output)

  , let input  = "test/golden/io-sim-classes.tags"
        output = "test/golden/io-sim-classes.tags.out"
    in goldenVsFile 
        "io-sim-classes tags"
         input
         output
         (parseGoldenFile input output)
 
  , let input  = "test/golden/ouroboros-network.tags"
        output = "test/golden/ouroboros-network.tags.out"
    in goldenVsFile 
        "ouroboros-network tags"
         input
         output
         (parseGoldenFile input output)

  , let input  = "test/golden/ouroboros-consensus.tags"
        output = "test/golden/ouroboros-consensus.tags.out"
    in goldenVsFile 
        "ouroboros-consensus tags"
         input
         output
         (parseGoldenFile input output)
  ]


parseGoldenFile :: FilePath -- input file
                -> FilePath -- output file
                -> IO ()
parseGoldenFile input output = do
    res <- withBinaryFile input ReadMode
      (BS.hGetContents >=> Vim.parseTagsFile . Text.decodeUtf8)
    case res of
      Left  err  -> throwIO (userError err)
      Right tags ->
        withBinaryFile output WriteMode
          $ flip BS.hPutBuilder (foldMap Vim.formatTag tags)
