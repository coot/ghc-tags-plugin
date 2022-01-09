{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Golden.Parser (tests) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad ((>=>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import           System.IO
import           System.Directory
import           System.FilePath

import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced

ext :: String
#if !defined(mingw32_HOST_OS)
ext = "posix"
#else
ext = "windows"
#endif


tests :: FilePath -> TestTree
tests goldenTestDir =
    testGroup "Golden.Parser" $
      [ testGroup "CTag"
          [ let input  = goldenTestDir </> "test.tags"
                golden = goldenTestDir </> "test.tags" <.> ext <.> "golden"
                output = goldenTestDir </> "test.tags.out"
            in goldenVsFile
                "test tags"
                 golden
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "vim.tags"
                golden = goldenTestDir </> "vim.tags" <.> ext <.> "golden"
                output = goldenTestDir </> "vim.tags.out"
            in goldenVsFile
                "vim tags"
                 golden
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "typed-protocols.tags"
                golden = goldenTestDir </> "typed-protocols.tags" <.> ext <.> "golden"
                output = goldenTestDir </> "typed-protocols.tags.out"
            in goldenVsFile
                "typed-protocols tags"
                 golden
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "io-sim-classes.tags"
                golden = goldenTestDir </> "io-sim-classes.tags" <.> ext <.> "golden"
                output = goldenTestDir </> "io-sim-classes.tags.out"
            in goldenVsFile
                "io-sim-classes tags"
                 golden
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "ouroboros-network.tags"
                golden = goldenTestDir </> "ouroboros-network.tags" <.> ext <.> "golden"
                output = goldenTestDir </> "ouroboros-network.tags.out"
            in goldenVsFile
                "ouroboros-network tags"
                 golden
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "ouroboros-consensus.tags"
                golden = goldenTestDir </> "ouroboros-consensus.tags" <.> ext <.> "golden"
                output = goldenTestDir </> "ouroboros-consensus.tags.out"
            in goldenVsFile
                "ouroboros-consensus tags"
                 golden
                 output
                 (parseGoldenCTagsFile input output)

          , let input  = goldenTestDir </> "ghc.tags"
                golden = goldenTestDir </> "ghc.tags" <.> ext <.> "golden"
                output = goldenTestDir </> "ghc.tags.out"
            in goldenVsFile
                "ghc tags"
                 golden
                 output
                 (parseGoldenCTagsFile input output)
          ]

      , testGroup "ETag"
#if MIN_VERSION_tasty_golden(2,3,4)
          [ let input  = goldenTestDir </> "ouroboros-consensus.ETAGS"
                golden = goldenTestDir </> "ouroboros-consensus.ETAGS" <.> ext <.> "golden"
                output = goldenTestDir </> "ouroboros-consensus.ETAGS.out"
            in localOption (SizeCutoff maxBound) $
               goldenVsFileVerbose
                "ouroboros-consensus TAGS"
                 golden
                 output
                 (parseGoldenETagsFile input output)

          ,
#else
          [
#endif
            let input  = goldenTestDir </> "vim.ETAGS"
                golden = goldenTestDir </> "vim.ETAGS" <.> ext <.> "golden"
                output = goldenTestDir </> "vim.ETAGS.out"
            in goldenVsFile
                "vim tags"
                 golden
                 output
                 (parseGoldenETagsFile input output)

          , let input  = goldenTestDir </> "ghc.ETAGS"
                golden = goldenTestDir </> "ghc.ETAGS" <.> ext <.> "golden"
                output = goldenTestDir </> "ghc.ETAGS.out"
            in goldenVsFile
                "ghc tags"
                 golden
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


#if MIN_VERSION_tasty_golden(2,3,4)
goldenVsFileVerbose
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> TestTree -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFileVerbose name ref new act =
  goldenTest2
    name
    (LBS.readFile ref)
    (act >> LBS.readFile new)
    cmp
    upd
    del
  where
    cmp :: LBS.ByteString -> LBS.ByteString -> IO (Maybe String)
    cmp  refBS newBS | refBS == newBS
                     = return Nothing
    cmp _refBS newBS = return (Just $ LBS.Char8.unpack newBS)

    upd = createDirectoriesAndWriteFile ref
    del = removeFile new
#endif
