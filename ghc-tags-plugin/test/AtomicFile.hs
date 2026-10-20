module Main (main) where

import Control.Concurrent
import Control.Exception
import System.Directory
import System.FilePath ((</>))
import System.IO

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit

import Plugin.GhcTags.AtomicFile

main :: IO ()
main = defaultMain $ testGroup "Atomic file updates"
    [ testCase "aborted initial update leaves no files" testAbortedInitialUpdate
    , testCase "successful update publishes replacement" testSuccessfulUpdate
    , testCase "aborted update preserves existing tags" testAbortedUpdate
    , testCase "write exception preserves existing tags" testWriteException
    , testCase "cancellation preserves tags and permits the next update" testCancellation
    ]


testAbortedInitialUpdate :: Assertion
testAbortedInitialUpdate = withTestDirectory $ \dir -> do
    withAtomicFile (dir </> "tags") $ \h ->
      hPutStr h "discarded\n" >> pure False
    listDirectory dir >>= (@?= [])


testSuccessfulUpdate :: Assertion
testSuccessfulUpdate = withTagsFile $ \dir path -> do
    withAtomicFile path $ \h -> hPutStr h "replacement\n" >> pure True
    checkTagsFile dir path "replacement\n"


testAbortedUpdate :: Assertion
testAbortedUpdate = withTagsFile $ \dir path -> do
    withAtomicFile path $ \h -> hPutStr h "partial\n" >> pure False
    checkTagsFile dir path original


testWriteException :: Assertion
testWriteException = withTagsFile $ \dir path -> do
    result <- try $ withAtomicFile path $ \h -> do
      hPutStr h "partial\n"
      hFlush h
      throwIO (userError "write failed")
    case result :: Either IOException () of
      Left _ -> pure ()
      Right _ -> assertFailure "write exception swallowed"
    checkTagsFile dir path original


testCancellation :: Assertion
testCancellation = withTagsFile $ \dir path -> do
    -- Cancel after partial output has reached the temporary file.
    started <- newEmptyMVar
    blocker <- newEmptyMVar :: IO (MVar ())
    finished <- newEmptyMVar
    let writer = do
          outcome <- try $ withAtomicFile path $ \h -> do
            hPutStr h "partial\n"
            hFlush h
            putMVar started ()
            takeMVar blocker
            pure True
          putMVar finished (outcome :: Either SomeException ())
    bracket (forkIO writer) killThread $ \tid -> do
      takeMVar started
      -- Readers see complete old contents while the update is in progress.
      readFile path >>= (@?= original)
      throwTo tid ThreadKilled
      outcome <- takeMVar finished
      case outcome of
        Left err -> fromException err @?= Just ThreadKilled
        Right _ -> assertFailure "cancellation swallowed"
      checkTagsFile dir path original
      withAtomicFile path $ \h -> hPutStr h "recovered\n" >> pure True
      checkTagsFile dir path "recovered\n"


--
-- Utils
--

original :: String
original = "existing tags\n"


withTestDirectory :: (FilePath -> IO a) -> IO a
withTestDirectory action = do
    tmp <- getTemporaryDirectory
    bracket (do (path, h) <- openTempFile tmp "ghc-tags-atomic-test"
                hClose h
                removeFile path
                createDirectory path
                pure path)
            removeDirectoryRecursive
            action

withTagsFile :: (FilePath -> FilePath -> IO a) -> IO a
withTagsFile action = withTestDirectory $ \dir -> do
    let path = dir </> "tags"
    writeFile path original
    action dir path

checkTagsFile :: FilePath -> FilePath -> String -> Assertion
checkTagsFile dir path expected = do
    readFile path >>= (@?= expected)
    listDirectory dir >>= (@?= ["tags"])
