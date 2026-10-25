{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Check (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isControl)
import Data.List ((\\), group, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing)
import System.FilePath (joinPath, splitDirectories, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Text ()

import GhcTags.Check


tests :: TestTree
tests = testGroup "Check"
  [ testProperty "duplicate declarations match grouped locations" duplicatesProp
  , testProperty "comparison matches list multiset difference" comparisonProp
  , testProperty "source line count matches splitting into lines" sourceLinesProp
  , testProperty "source diagnostics match the generated files" sourceChecksProp
  , testProperty "source paths are relative to each tags file" relativePathsProp
  ]


newtype ArbLocation = ArbLocation { getTagLocation :: TagLocation }
  deriving Show

instance Arbitrary ArbLocation where
    arbitrary = fmap ArbLocation $ TagLocation
      <$> elements sourcePaths
      <*> frequency [ (5, Just <$> choose (-2, 8))
                    , (1, pure Nothing)
                    ]
      <*> frequency [ (4, elements ["foo", "bar", "baz"])
                    , (1, fixName <$> arbitrary)
                    ]
    shrink (ArbLocation tag@TagLocation {sourceLine, sourceName}) =
         [ArbLocation tag {sourceLine = line} | line <- shrink sourceLine]
      ++ [ ArbLocation tag {sourceName = name}
         | name <- map fixName $ shrink sourceName
         , name /= sourceName
         ]


fixName :: Text.Text -> Text.Text
fixName name = case Text.filter (not . isControl) name of
    "" -> "a"
    valid -> valid


sourcePaths :: [FilePath]
sourcePaths = ["A.hs", "source with spaces/B.hs", "sub/C.hs"]


newtype Locations = Locations [TagLocation]
  deriving Show

instance Arbitrary Locations where
    arbitrary = Locations . map getTagLocation <$> resize 30 arbitrary
    shrink (Locations tags) = map Locations $ shrinkList
      (\tag -> [smaller | ArbLocation smaller <- shrink (ArbLocation tag)]) tags


modelDeclarations :: [TagLocation] -> [Declaration]
modelDeclarations =
    mapMaybe $
      \TagLocation {sourcePath, sourceLine, sourceName} ->
      (\line -> Declaration sourcePath line sourceName) <$> sourceLine


duplicatesProp :: Locations -> Property
duplicatesProp (Locations tags) =
    let groups = group $ sort $ modelDeclarations tags
        expected = [(location, length locations) | locations@(location : _ : _) <- groups]
    in Map.toAscList (duplicateDeclarations tags) === expected


comparisonProp :: Locations -> Locations -> Property
comparisonProp (Locations left) (Locations right) =
    let a = modelDeclarations left
        b = modelDeclarations right
        expected = (sort $ a \\ b, sort $ b \\ a)
        (onlyLeft, onlyRight) = compareDeclarations left right
        expand counts = concat [replicate count location | (location, count) <- Map.toAscList counts]
    in (expand onlyLeft, expand onlyRight) === expected


newtype Contents = Contents BS.ByteString
  deriving Show

instance Arbitrary Contents where
    arbitrary = Contents <$> frequency
      [ (2, arbitrary)
      , (1, pure BS.empty)
      , (3, BS.intercalate "\n" <$> resize 6 arbitrary)
      , (1, (<> "\n") <$> arbitrary)
      ]
    shrink (Contents bytes) = map Contents $ shrink bytes


sourceLinesProp :: Contents -> Property
sourceLinesProp (Contents bytes) = countSourceLines bytes === length (BSC.lines bytes)


data SourceFile = MissingFile | ReadableFile BS.ByteString | SourceDirectory
  deriving (Eq, Show)

instance Arbitrary SourceFile where
    arbitrary = frequency
      [ (2, pure MissingFile)
      , (5, (\(Contents bytes) -> ReadableFile bytes) <$> arbitrary)
      , (1, pure SourceDirectory)
      ]


data SourceExample = SourceExample
  { sourceFiles :: Map.Map FilePath SourceFile
  , sourceTags :: Locations
  }
  deriving Show

instance Arbitrary SourceExample where
    arbitrary = SourceExample
      <$> (Map.fromList <$> resize 6 (listOf $ (,)
            <$> elements sourcePaths
            <*> arbitrary))
      <*> arbitrary
    shrink SourceExample {sourceFiles, sourceTags} =
         [ SourceExample (Map.fromList smaller) sourceTags
         | smaller <- shrinkList (const []) (Map.toList sourceFiles)
         ]
      ++ [SourceExample sourceFiles smaller | smaller <- shrink sourceTags]


sourceChecksProp :: SourceExample -> Property
sourceChecksProp SourceExample {sourceFiles, sourceTags = Locations tags} = ioProperty $
    withSystemTempDirectory "gtp-check-sources" $ \root -> do
      forM_ (Map.toList sourceFiles) $ \(path, contents) ->
        case contents of
          MissingFile -> pure ()
          ReadableFile bytes -> do
            createDirectoryIfMissing True (takeDirectory $ root </> path)
            BS.writeFile (root </> path) bytes
          SourceDirectory -> do
            createDirectoryIfMissing True (takeDirectory $ root </> path)
            createDirectory (root </> path)
      let absoluteTags = [tag {sourcePath = root </> sourcePath tag} | tag <- tags]
          paths = sort $ nub $ map sourcePath tags
          expectedMissing =
            [root </> path | path <- paths, Map.lookup path sourceFiles `elem` [Nothing, Just MissingFile]]
          expectedUnreadable =
            [root </> path | path <- paths, Map.lookup path sourceFiles == Just SourceDirectory]
          expectedInvalid =
            [ (Declaration (root </> path) line name, length $ BSC.lines bytes)
            | Declaration path line name <- sort $ nub $ modelDeclarations tags
            , Just (ReadableFile bytes) <- [Map.lookup path sourceFiles]
            , line < 1 || line > length (BSC.lines bytes)
            ]
          expected = (expectedMissing, expectedUnreadable, expectedInvalid)
      actual <- checkSources absoluteTags
      pure $ (missingSources actual, map fst $ unreadableSources actual, invalidLines actual) === expected


data PathExample = PathExample FilePath FilePath
  deriving Show

instance Arbitrary PathExample where
    arbitrary = PathExample
      <$> (joinPath . ("sources" :) <$> resize 4 (listOf segment))
      <*> (joinPath . ("output" :) <$> resize 4 (listOf segment))
      where
        segment = elements ["a", "b", "directory with spaces"]


relativePathsProp :: PathExample -> Property
relativePathsProp (PathExample sourceDir tagsDir) = ioProperty $
    withSystemTempDirectory "gtp-check-paths" $ \root -> do
      let source = root </> sourceDir </> "A.hs"
          output = root </> tagsDir
          relative = joinPath (replicate (length $ splitDirectories tagsDir) "..")
                   </> sourceDir </> "A.hs"
          tag = TagLocation source (Just 1) "foo"
      createDirectoryIfMissing True (takeDirectory source)
      createDirectoryIfMissing True output
      BS.writeFile source "foo = ()\n"
      expected <- canonicalizePath source
      fromRelative <- resolveTagPaths output [tag {sourcePath = relative}]
      fromAbsolute <- resolveTagPaths root [tag]
      pure $ (fromRelative, fromAbsolute) === ([tag {sourcePath = expected}], [tag {sourcePath = expected}])
