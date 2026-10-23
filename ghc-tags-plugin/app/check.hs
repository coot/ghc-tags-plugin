{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Inspect a consistent snapshot of a ctags or etags file.
module Main where

import           Control.Exception (IOException, displayException, handle)
import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.Either (partitionEithers)
import qualified Data.Text as Text
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.FilePath (splitFileName, (</>))
import           System.IO (hPutStrLn, stderr)

import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag

import           Plugin.GhcTags.FileLock


data Options = Options
  { etags    :: Bool
  , filePath :: FilePath
  }


optionsParser :: Parser Options
optionsParser = Options
    <$> switch (  long "etags"
               <> short 'e'
               <> help "read an emacs etags file")
    <*> strArgument (  metavar "FILE"
                    <> help "tags file to inspect"
                    <> showDefault
                    <> value "tags"
                    )


main :: IO ()
main = do
    options <- execParser $
      info (optionsParser <**> helper)
        (fullDesc <> progDesc "Parse a tags file and report metadata, counts and diagnostics")
    handle reportIOException (checkFile options)


checkFile :: Options -> IO ()
checkFile Options { etags, filePath } = do
    bytes <- withFileLock_ (lockFile filePath) SharedLock (BS.readFile filePath)
    if etags
      then do
        tags <- ETag.parseTagsFileRecovering bytes
            >>= reportParseResult
        putStrLn ("Tags: " ++ show (length tags))
      else do
        parsed <- CTag.parseTagsFileRecovering bytes
              >>= reportParseResult
        let (headers, tags) = partitionEithers parsed
        mapM_ reportHeader headers
        putStrLn ("Tags: " ++ show (length tags))
        when (any (\(a, b) -> CTag.compareTags a b == GT) (zip tags (drop 1 tags))) $
          reportWarning "ctags are not sorted"


reportHeader :: CTag.Header -> IO ()
reportHeader (CTag.Header CTag.ProgramName _ name _) =
    putStrLn ("Program: " ++ Text.unpack name)
reportHeader (CTag.Header CTag.ProgramVersion _ version _) =
    putStrLn ("Version: " ++ Text.unpack version)
reportHeader _ = pure ()


reportParseResult :: Either String ([String], a) -> IO a
reportParseResult result = case result of
    Left err -> reportError err
    Right (warnings, parsed) -> do
      mapM_ reportWarning warnings
      pure parsed


reportWarning :: String -> IO ()
reportWarning message = hPutStrLn stderr ("Warning: " ++ message)


reportError :: String -> IO a
reportError message = do
    hPutStrLn stderr ("Error: " ++ message)
    exitFailure


reportIOException :: IOException -> IO ()
reportIOException = reportError . displayException


lockFile :: FilePath -> FilePath
lockFile file = case splitFileName file of
    (dir, name) -> dir </> "." ++ name ++ ".lock"
