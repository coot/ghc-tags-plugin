{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

-- | Format-independent inspection of declaration locations.
module GhcTags.Check
  ( TagLocation (..)
  , Declaration (..)
  , SourceReport (..)
  , tagLocation
  , resolveTagPaths
  , declarationCounts
  , duplicateDeclarations
  , compareDeclarations
  , countSourceLines
  , checkSources
  ) where

import Control.Exception (IOException, displayException, evaluate, try)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, makeAbsolute)
import System.FilePath (normalise, (</>))
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

import GhcTags.Tag
  ( Tag (..), TagAddress (..), TagField (..), TagFields (..)
  , TagFilePath (..), TagName (..)
  )


data TagLocation = TagLocation
  { sourcePath :: FilePath
  , sourceLine :: Maybe Int
  , sourceName :: Text
  }
  deriving (Eq, Ord, Show)

-- | The information shared by ctags and etags. Kind, column and byte offset
-- are deliberately absent: they cannot be compared across the two formats.
data Declaration = Declaration
  { declarationPath :: FilePath
  , declarationLine :: Int
  , declarationName :: Text
  }
  deriving (Eq, Ord, Show)

data SourceReport = SourceReport
  { missingSources    :: [FilePath]
  , unreadableSources :: [(FilePath, String)]
  , invalidLines      :: [(Declaration, Int)]
    -- ^ Declaration and the number of lines in its source file.
  }
  deriving (Eq, Show)


tagLocation :: Tag tk -> TagLocation
tagLocation tag = TagLocation
  { sourcePath = Text.unpack $ getRawFilePath $ tagFilePath tag
  , sourceLine = case tagAddr tag of
      TagLine line      -> Just line
      TagLineCol line _ -> Just line
      NoAddress         -> Nothing
      TagCommand _      -> case tagFields tag of
        TagFields fields -> case mapMaybe lineField fields of
          line : _ -> Just line
          []       -> Nothing
  , sourceName = getTagName $ tagName tag
  }
  where
    lineField TagField {fieldName, fieldValue}
      | fieldName == Text.pack "line" = readMaybe $ Text.unpack fieldValue
      | otherwise = Nothing


-- | Resolve each distinct path once, relative to the supplied source root.
-- Canonicalization also handles symlinks.
resolveTagPaths :: FilePath -> [TagLocation] -> IO [TagLocation]
resolveTagPaths sourceRoot tags = do
    directory <- makeAbsolute sourceRoot
    paths <- forM (Map.keys $ Map.fromList [(sourcePath tag, ()) | tag <- tags]) $ \path -> do
      let absolute = directory </> path
      result <- try (canonicalizePath absolute) :: IO (Either IOException FilePath)
      -- Preserve a usable path for read diagnostics if canonicalization fails.
      pure (path, either (const $ normalise absolute) id result)
    let resolvedPaths = Map.fromList paths
    pure [tag {sourcePath = resolvedPaths Map.! sourcePath tag} | tag <- tags]


declaration :: TagLocation -> Maybe Declaration
declaration TagLocation {sourcePath, sourceLine, sourceName} =
    (\line -> Declaration sourcePath line sourceName) <$> sourceLine


declarationCounts :: [TagLocation] -> Map Declaration Int
declarationCounts = Map.fromListWith (+) . map (\location -> (location, 1))
                  . mapMaybe declaration


duplicateDeclarations :: [TagLocation] -> Map Declaration Int
duplicateDeclarations = Map.filter (> 1) . declarationCounts


-- | Multiset differences, preserving how many copies occur only on each side.
compareDeclarations :: [TagLocation]
                    -> [TagLocation]
                    -> (Map Declaration Int, Map Declaration Int)
compareDeclarations left right =
    ( leftCounts `difference` rightCounts
    , rightCounts `difference` leftCounts
    )
  where
    leftCounts  = declarationCounts left
    rightCounts = declarationCounts right
    difference  = Map.differenceWith $
      \a b -> if a > b then Just (a - b) else Nothing


-- | Count lines without inventing a trailing empty line after a final LF.
-- CRLF files use the same count. An empty file has no valid line numbers.
countSourceLines :: BS.ByteString -> Int
countSourceLines bytes = BSC.count '\n' bytes
                       + if BS.null bytes || BSC.last bytes == '\n'
                           then 0 else 1


-- | Read each source file once. Failed reads remain distinct from missing
-- files; line bounds can only be checked for successfully read sources.
checkSources :: [TagLocation] -> IO SourceReport
checkSources tags = do
    sources <- forM paths $ \path -> do
      result <- try @IOException (BS.readFile path >>= evaluate . countSourceLines)
      pure (path, result)
    let (missing, unreadable, lineCounts) =
          List.foldl' collect ([], [], Map.empty) sources
        badLines =
          [ (location, linesInFile)
          | location@Declaration
              { declarationPath,
                declarationLine
              } <- Map.keys $ declarationCounts tags
          , linesInFile <- maybeToList (Map.lookup declarationPath lineCounts)
          , declarationLine < 1 || declarationLine > linesInFile
          ]
    pure SourceReport
      { missingSources    = reverse missing
      , unreadableSources = reverse unreadable
      , invalidLines      = badLines
      }
  where
    paths = Map.keys $ Map.fromList [(sourcePath tag, ()) | tag <- tags]

    collect :: ([FilePath], [(FilePath, String)], Map FilePath Int)
            -> (FilePath, Either IOException Int)
            -> ([FilePath], [(FilePath, String)], Map FilePath Int)
    collect (missing, unreadable, counts) = \case
      (path, Right linesInFile)
        -> (missing, unreadable, Map.insert path linesInFile counts)

      (path, Left err)
        |  isDoesNotExistError err
        -> (path : missing, unreadable, counts)

        |  otherwise
        -> (missing, (path, displayException err) : unreadable, counts)
