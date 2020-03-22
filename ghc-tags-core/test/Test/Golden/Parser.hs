{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Parser (tests) where

import           Algebra.Lattice ((/\))

import           Control.Arrow
import           Control.Exception
import           Control.Monad ((>=>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Encoding as Text
import           System.IO
import           System.FilePath
import           Text.Printf

import qualified GhcTags.CTags as CTags
import qualified GhcTags.ETags as ETags

import           Test.Tasty (TestTree, TestName, testGroup)
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced


tests :: FilePath -> TestTree
tests goldenTestDir =
    testGroup "Golden.Parser" $
      [ testGroup "CTags"
          [ let input  = goldenTestDir </> "test.tags"
                output = goldenTestDir </> "test.tags.out"
            in goldenVsFileIgnoreHeaders
                "test tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "vim.tags"
                output = goldenTestDir </> "vim.tags.out"
            in goldenVsFileIgnoreHeaders
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
            in goldenVsFileIgnoreHeaders
                "ghc tags"
                 input
                 output
                 (parseGoldenCTagsFile input output)
          ]

      , testGroup "ETags"
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

--
--
--

-- | Compare the output file's contents against the golden file's contents
-- after the given action has created the output file.
goldenVsFileIgnoreHeaders
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> TestTree -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFileIgnoreHeaders name ref new act =
  goldenTest
    name
    (readFileStrict ref)
    (act >> readFileStrict new)
    cmp
    upd
  where
  upd = createDirectoriesAndWriteFile ref
  cmp :: BSL.ByteString -> BSL.ByteString -> IO (Maybe String)
  cmp b0 b1 = if filterHeaders b0 /= filterHeaders b1
                then pure $ Just $ printf "Files '%s' and '%s' differ" ref new
                else pure Nothing

  -- filter headers, but only from the first 10 lnes
  filterHeaders :: BSL.ByteString -> BSL.ByteString
  filterHeaders = BLC.unlines
                . uncurry (++)
                . first (filter (not . BSL.isPrefixOf "!_TAG_FILE_"
                              /\ not . BSL.isPrefixOf "!_TAG_PROGRAM_"))
                . splitAt 20
                . BLC.lines

  
readFileStrict :: FilePath -> IO BSL.ByteString
readFileStrict path = do
  s <- BSL.readFile path
  evaluate $ BSL.foldr seq () s
  return s
