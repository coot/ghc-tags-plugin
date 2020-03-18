module Test.Golden.Parser (tests) where

import           Control.Exception
import           Control.Monad ((>=>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Encoding as Text
import           System.IO
import           Text.Printf

import qualified Plugin.GhcTags.CTags as CTags
import           Plugin.GhcTags.Utils (endOfLine)

import           Test.Tasty (TestName, TestTree, testGroup)
import           Test.Tasty.Golden (createDirectoriesAndWriteFile)
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

  , let input  = "test/golden/vim.tags"
        output = "test/golden/vim.tags.out"
    in goldenVsFile 
        "vim tags"
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
      (BS.hGetContents >=> CTags.parseTagsFile . Text.decodeUtf8)
    case res of
      Left  err  -> throwIO (userError err)
      Right tags ->
        withBinaryFile output WriteMode
          $ flip BS.hPutBuilder (foldMap CTags.formatTag tags)


-- | Convert the '\n' to 'endOfLine' in the reference file and compare with the
-- result.
--
goldenVsFile
  :: TestName
  -> FilePath
  -> FilePath
  -> IO ()
  -> TestTree
goldenVsFile name ref new act =
    goldenTest
      name
      (BSL.fromStrict <$> BS.readFile ref)
      (act >> BSL.fromStrict <$> BS.readFile new)
      (\ref' new' ->
        pure $
          if convert ref' == new'
            then Nothing
            else Just $ printf "Files '%s' and '%s' differ" ref new)
      upd
  where
    convert :: BSL.ByteString -> BSL.ByteString
    convert = BSC.foldr (\c cs -> case c of
                          '\n' -> BSC.pack endOfLine <> cs
                          _    -> BSC.cons c cs)
                        mempty

    upd = createDirectoriesAndWriteFile ref
