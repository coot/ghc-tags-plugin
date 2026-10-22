{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tag.Paths (tests) where

import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity (..))
import           Data.List (groupBy, nub, sortBy)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Pipes
import qualified Pipes.Prelude as Pipes

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import qualified GhcTags.CTag as CTag
import           GhcTags.Stream (combineTagsStream)
import           GhcTags.Tag

import           Test.Tag.Generators (genField, genTagKind, shrinkTag')


type MergeTags = RawFilePath -> [CTag] -> [CTag] -> [CTag]


tests :: TestTree
tests = testGroup "Tag file identity"
    [ testProperty "non-streaming matches model" (propCombineTags (combineTags CTag.compareTags))
    , testProperty "streaming matches model" (propCombineTags mergeTagsPipe)
    ]


-- New tags belong to one module; old tags may span several modules.
-- Both lists are sorted, and may contain duplicate tags.
data TagUpdate = TagUpdate
    { modulePath :: TagFilePath
    , oldTags    :: [CTag]
    , newTags    :: [CTag]
    }
    deriving Show

instance Arbitrary TagUpdate where
    arbitrary = do
      file <- (<> ".hs") <$> genPathComponent
      prefix <- genPathComponent
      source <- elements [file, "src/" <> file, "one/src/" <> file]
      let path = makePath source
          -- Include collisions deliberately instead of relying on random
          -- filenames to happen to share a suffix.
          oldPaths = map makePath
            [ source
            , prefix <> source
            , "nested/" <> source
            , "two/src/" <> file
            , "other/" <> file
            ]
      old <- genTagList (elements oldPaths)
      new <- genTagList (pure path)
      pure $ TagUpdate path old new

    shrink update@TagUpdate { oldTags, newTags } =
      [ update { oldTags = sortTags smaller }
      | smaller <- shrinkList shrinkTag' oldTags
      ]
      ++
      [ update { newTags = sortTags smaller }
      | smaller <- shrinkList shrinkTag' newTags
      ]


-- An independent specification: remove the old module, add its new tags,
-- sort the result and remove duplicates. It uses neither production merge function.
modelCombineTags :: TagUpdate -> [CTag]
modelCombineTags TagUpdate { modulePath, oldTags, newTags } =
    nub . sortBy CTag.compareTags $
      newTags ++ filter ((/= modulePath) . tagFilePath) oldTags


propCombineTags :: MergeTags -> TagUpdate -> Property
propCombineTags merge update@TagUpdate { modulePath, oldTags, newTags } =
    checkCoverage $
      cover 10 (null oldTags) "empty old tags" $
      cover 10 (null newTags) "empty new tags" $
      cover 10 (hasDuplicates newTags) "duplicate new tags" $
      cover 10 (hasDistinctEqualKeys newTags || hasDistinctEqualKeys oldTags) "distinct tags with equal sorting keys" $
      cover 10 (hasDuplicates $ filter ((/= modulePath) . tagFilePath) oldTags) "duplicate retained old tags" $
      cover 10 (any ((== modulePath) . tagFilePath) oldTags) "replaces existing module" $
      cover 10 (any suffixCollision oldTags) "different file with matching suffix" $

        merge (rawFilePathFromBS $ Text.encodeUtf8 $ getRawFilePath modulePath)
              newTags oldTags
        ===
        modelCombineTags update
  where
    suffixCollision tag =
         tagFilePath tag /= modulePath
      && getRawFilePath modulePath `Text.isSuffixOf` getRawFilePath (tagFilePath tag)


genPathComponent :: Gen Text
genPathComponent = Text.pack <$> resize 8 (listOf1 (elements ['a'..'z']))


genTagList :: Gen TagFilePath -> Gen [CTag]
genTagList path = sortTags <$> frequency
    [ (1, pure [])
    , (2, listOf1 (genTag path))
    , (2, do tags <- listOf1 (genTag path)
             tag <- elements tags
             pure (tag : tags))
    , (1, do tag <- genTag path
             let first = tag { tagFields = TagFields [TagField "test" "first"] }
                 second = tag { tagFields = TagFields [TagField "test" "second"] }
             pure [first, second, first])
    ]


genTag :: Gen TagFilePath -> Gen CTag
genTag path =
    Tag <$> (TagName <$> elements ["alpha", "beta", "gamma"])
        <*> genTagKind SingCTag
        <*> path
        <*> (TagLine <$> choose (1, 10))
        <*> pure NoTagDefinition
        <*> (TagFields <$> resize 3 (listOf genField))


sortTags :: [CTag] -> [CTag]
sortTags = sortBy CTag.compareTags


hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates tags = length tags /= length (nub tags)


hasDistinctEqualKeys :: [CTag] -> Bool
hasDistinctEqualKeys =
    any ((> 1) . length . nub)
  . groupBy (\a b -> CTag.compareTags a b == EQ)


makePath :: Text -> TagFilePath
makePath = TagFilePath
         . Text.decodeUtf8 . rawFilePathToBS
         . normaliseRawFilePath
         . rawFilePathFromBS . Text.encodeUtf8


mergeTagsPipe :: MergeTags
mergeTagsPipe path new old = runIdentity $ Pipes.toListM $
    combineTagsStream CTag.compareTags path new (traverse_ Pipes.yield old)
