module Test.Golden.Parser (tests) where

import           Control.Exception
import           Control.Monad ((>=>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Encoding as Text
import           System.IO

import qualified Plugin.GhcTags.CTags as CTags
import qualified Plugin.GhcTags.ETags as ETags

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden


tests :: TestTree
tests =
    testGroup "Golden.Parser" $
      [ testGroup "CTags"
          [ let input  = "test/golden/test.tags"
                output = "test/golden/test.tags.out"
            in goldenVsFile 
                "test tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = "test/golden/vim.tags"
                output = "test/golden/vim.tags.out"
            in goldenVsFile 
                "vim tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = "test/golden/typed-protocols.tags"
                output = "test/golden/typed-protocols.tags.out"
            in goldenVsFile 
                "typed-protocols tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = "test/golden/io-sim-classes.tags"
                output = "test/golden/io-sim-classes.tags.out"
            in goldenVsFile 
                "io-sim-classes tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)
         
          , let input  = "test/golden/ouroboros-network.tags"
                output = "test/golden/ouroboros-network.tags.out"
            in goldenVsFile 
                "ouroboros-network tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = "test/golden/ouroboros-consensus.tags"
                output = "test/golden/ouroboros-consensus.tags.out"
            in goldenVsFile 
                "ouroboros-consensus tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = "test/golden/ghc.tags"
                output = "test/golden/ghc.tags.out"
            in goldenVsFile 
                "ghc tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)
          ]

      , testGroup "ETags"
          [ let input  = "test/golden/ouroboros-consensus.ETAGS"
                output = "test/golden/ouroboros-consensus.ETAGS.out"
            in goldenVsFile 
                "ouroboros-consensus TAGS"
                 input
                 output
                 (parseGoldenETagsFile input output)

          , let input  = "test/golden/vim.ETAGS"
                output = "test/golden/vim.ETAGS.out"
            in goldenVsFile 
                "vim tags"
                 input
                 output
                 (parseGoldenETagsFile input output)

          , let input  = "test/golden/ghc.ETAGS"
                output = "test/golden/ghc.ETAGS.out"
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
      (BS.hGetContents >=> CTags.parseTagsFile . Text.decodeUtf8)
    case res of
      Left  err  -> throwIO (userError err)
      Right tags ->
        withBinaryFile output WriteMode
          $ flip BS.hPutBuilder (foldMap CTags.formatTag tags)


parseGoldenETagsFile
    :: FilePath -- ^ input file
    -> FilePath -- ^ output file
    -> IO ()
parseGoldenETagsFile input output = do
    res <- withBinaryFile input ReadMode
      (BS.hGetContents >=> ETags.parseTagsFile . Text.decodeUtf8)
    case res of
      Left  err  -> throwIO (userError err)
      Right tags ->
        withBinaryFile output WriteMode
          $ flip BS.hPutBuilder (ETags.formatETagsFile tags)
