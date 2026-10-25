{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Inspect a consistent snapshot of a ctags or etags file.
module Main where

import           Control.Exception (IOException, displayException, handle)
import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.FilePath (splitFileName, takeDirectory, (</>))
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)

import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag
import qualified GhcTags.Check as Check
import           GhcTags.Tag (tagName)

import           Plugin.GhcTags.FileLock


data Options = Options
  { etags       :: Bool
  , filePath    :: FilePath
  , compareFile :: Maybe FilePath
  , sourceRoot  :: Maybe FilePath
  , maxExamples :: Int
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
    <*> optional (strOption
          ( long "compare"
         <> metavar "FILE"
         <> help "compare declaration locations with FILE (ctags/etags detected automatically)"))
    <*> optional (strOption
          ( long "source-root"
         <> metavar "DIR"
         <> help "resolve source paths in both files relative to DIR (default: each tags file's directory)"))
    <*> option (eitherReader readLimit)
          ( long "max-examples"
         <> metavar "N"
         <> value 10
         <> showDefault
         <> help "maximum examples per diagnostic category; 0 shows all")
  where
    readLimit inputText = case readMaybe inputText of
      Just n | n >= 0 -> Right n
      _ -> Left "expected a non-negative integer"


data Format = CTags | ETags
  deriving (Eq, Show)

data ParsedFile = ParsedFile
  { parsedPath :: FilePath
  , parsedLocations :: [Check.TagLocation]
  }


main :: IO ()
main = do
    options <- execParser $
      info (optionsParser <**> helper)
        (fullDesc <> progDesc "Parse a tags file and report metadata, counts and diagnostics")
    handle reportIOException (checkFiles options)


checkFiles :: Options -> IO ()
checkFiles Options { etags,
                     filePath,
                     compareFile,
                     sourceRoot,
                     maxExamples }
    = do
    primary <- inspectFile maxExamples sourceRoot (Just $ if etags then ETags else CTags) filePath
    case compareFile of
      Nothing -> pure ()
      Just otherPath -> do
        other <- inspectFile maxExamples sourceRoot Nothing otherPath
        reportComparison maxExamples primary other


inspectFile :: Int -> Maybe FilePath -> Maybe Format -> FilePath -> IO ParsedFile
inspectFile limit sourceRootOverride requestedFormat path = do
    bytes <- withFileLock_ (lockFile path) SharedLock (BS.readFile path)
    let format = maybe (detectFormat bytes) id requestedFormat
        warning message = reportWarning (path ++ ": " ++ message)

        parseResult :: Either String ([String], a) -> IO a
        parseResult result = case result of
          Left err -> reportError (path ++ ": " ++ err)
          Right (warnings, parsed) -> do
            reportExamples limit (path ++ ": parse warnings") [path ++ ": " ++ message | message <- warnings]
            pure parsed
    putStrLn ("File: " ++ path ++ if format == ETags then " (etags)" else " (ctags)")
    locations <- case format of
      ETags -> do
        tags <- ETag.parseTagsFileRecovering bytes >>= parseResult
        pure $ map Check.tagLocation tags
      CTags -> do
        parsed <- CTag.parseTagsFileRecovering bytes >>= parseResult
        let (headers, tags) = partitionEithers parsed
            adjacent = zip tags (drop 1 tags)
        mapM_ reportHeader headers
        if any (\(a, b) -> tagName a > tagName b) adjacent
          then warning "ctags are not sorted by name"
          else when (any (\(a, b) -> CTag.compareTags a b == GT) adjacent) $
            warning "ctags names are sorted, but secondary ordering differs from ghc-tags-plugin"
        pure $ map Check.tagLocation tags
    resolved <- Check.resolveTagPaths (fromMaybe (takeDirectory path) sourceRootOverride) locations
    let duplicates = Check.duplicateDeclarations resolved
        sourceFiles = Map.fromList [(Check.sourcePath tag, ()) | tag <- resolved]
        unlocated = length $ filter ((== Nothing) . Check.sourceLine) resolved
    putStrLn ("Tags: " ++ show (length resolved))
    putStrLn ("Source files: " ++ show (Map.size sourceFiles))
    putStrLn ("Declaration locations: " ++ show (Map.size $ Check.declarationCounts resolved))
    putStrLn ("Duplicate declaration locations: " ++ show (Map.size duplicates)
           ++ " (" ++ show (sum $ map (subtract 1) $ Map.elems duplicates) ++ " extra tags)")
    reportExamples limit (path ++ ": duplicate locations")
      [ path ++ ": duplicate declaration location: " ++ displayDeclaration location
        ++ " (" ++ show count ++ " tags)"
      | (location, count) <- Map.toAscList duplicates
      ]
    when (unlocated > 0) $ warning $
      show unlocated ++ " tags have no numeric line address; excluded from line checks and declaration comparison"
    sources <- Check.checkSources resolved
    reportSources limit path sources
    pure ParsedFile {parsedPath = path, parsedLocations = resolved}


detectFormat :: BS.ByteString -> Format
detectFormat bytes = case BS.uncons $ BS.dropWhile (`elem` [9, 10, 13, 32]) bytes of
    Just (12, _) -> ETags
    _            -> CTags


reportSources :: Int -> FilePath -> Check.SourceReport -> IO ()
reportSources limit path Check.SourceReport
  {Check.missingSources, Check.unreadableSources, Check.invalidLines} = do
    putStrLn ("Missing source files: " ++ show (length missingSources))
    putStrLn ("Unreadable source files: " ++ show (length unreadableSources))
    putStrLn ("Invalid line numbers: " ++ show (length invalidLines))
    reportExamples limit (path ++ ": missing files")
      [path ++ ": source file does not exist: " ++ source | source <- missingSources]
    reportExamples limit (path ++ ": unreadable files")
      [path ++ ": cannot read source file " ++ source ++ ": " ++ err | (source, err) <- unreadableSources]
    reportExamples limit (path ++ ": invalid line numbers")
      [ path ++ ": invalid line number: " ++ displayDeclaration location
        ++ " (source has " ++ show count ++ " lines)"
      | (location, count) <- invalidLines
      ]


reportComparison :: Int -> ParsedFile -> ParsedFile -> IO ()
reportComparison limit left right = do
    let (onlyLeft, onlyRight) = Check.compareDeclarations (parsedLocations left) (parsedLocations right)
        reportOnly file difference = do
          putStrLn ("Only in " ++ file ++ ": " ++ show (sum $ Map.elems difference)
                 ++ " tags at " ++ show (Map.size difference) ++ " declaration locations")
          reportExamples limit ("only in " ++ file)
            [ "only in " ++ file ++ ": " ++ displayDeclaration location
              ++ " (" ++ show count ++ " tags)"
            | (location, count) <- Map.toAscList difference
            ]
    putStrLn ("Comparison: " ++ parsedPath left ++ " vs " ++ parsedPath right)
    reportOnly (parsedPath left) onlyLeft
    reportOnly (parsedPath right) onlyRight
    if Map.null onlyLeft && Map.null onlyRight
      then putStrLn "Comparable declaration locations match (including duplicate counts)."
      else exitFailure


displayDeclaration :: Check.Declaration -> String
displayDeclaration Check.Declaration {Check.declarationPath, Check.declarationLine, Check.declarationName} =
    declarationPath ++ ":" ++ show declarationLine ++ ": " ++ show (Text.unpack declarationName)


reportExamples :: Int -> String -> [String] -> IO ()
reportExamples limit category examples = do
    let shown = if limit == 0 then examples else take limit examples
        remaining = length examples - length shown
    mapM_ reportWarning shown
    when (remaining > 0) $ reportWarning $
      category ++ ": " ++ show remaining ++ " more (use --max-examples 0 to show all)"


reportHeader :: CTag.Header -> IO ()
reportHeader CTag.Header { CTag.headerType,
                           CTag.headerArg,
                           CTag.headerComment } =
    case headerType of
      CTag.ProgramName ->
        putStrLn (withComment $ "Program: " ++ Text.unpack headerArg)
      CTag.ProgramVersion ->
        putStrLn (withComment $ "Version: " ++ Text.unpack headerArg)
      CTag.FileFormat ->
        putStrLn (withComment $ "FileFormat: " ++ show headerArg)
      CTag.FileSorted ->
        putStrLn (withComment $ "FileSorted: " ++ show headerArg)
      _ -> pure ()

  where
    withComment :: String -> String
    withComment a
      | not (Text.null headerComment)
      = a ++ " (" ++  Text.unpack headerComment ++ ")"
      | otherwise
      = a


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
