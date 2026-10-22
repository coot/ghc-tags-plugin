{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test.Tag.Paths (tests) where

import           Control.Monad.State.Strict (runStateT)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity (..))
import           Data.List (nub, sortBy)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Pipes
import qualified Pipes.Lift as Pipes
import qualified Pipes.Prelude as Pipes

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import qualified GhcTags.CTag as CTag
import           GhcTags.Stream (combineTagsPipe)
import           GhcTags.Tag

import           Test.Tag.Generators (genField, genTagKind, shrinkTag')


type MergeTags = RawFilePath -> [CTag] -> [CTag] -> [CTag]


tests :: TestTree
tests = testGroup "Tag file identity"
    [ testProperty "non-streaming matches model" (propCombineTags (combineTags CTag.compareTags))
    , testProperty "streaming matches model" (propCombineTags mergeTagsPipe)
    ]


-- New tags belong to one module; old tags may span several modules.
-- Both lists are sorted and duplicate-free, including after shrinking.
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
-- and sort the result. It does not use either production merge function.
modelCombineTags :: TagUpdate -> [CTag]
modelCombineTags TagUpdate { modulePath, oldTags, newTags } =
    sortBy CTag.compareTags $
      newTags ++ filter ((/= modulePath) . tagFilePath) oldTags


propCombineTags :: MergeTags -> TagUpdate -> Property
propCombineTags merge update@TagUpdate { modulePath, oldTags, newTags } =
    checkCoverage $
      cover 10 (null oldTags) "empty old tags" $
      cover 10 (null newTags) "empty new tags" $
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
    , (4, listOf1 (genTag path))
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
sortTags = nub . sortBy CTag.compareTags


makePath :: Text -> TagFilePath
makePath = TagFilePath
         . Text.decodeUtf8 . rawFilePathToBS
         . normaliseRawFilePath
         . rawFilePathFromBS . Text.encodeUtf8


mergeTagsPipe :: MergeTags
mergeTagsPipe path new old =
    case runStateT
           (Pipes.toListM
             (Pipes.for
               (traverse_ Pipes.yield old)
               (\tag -> Pipes.stateP $ fmap ((),) . combineTagsPipe CTag.compareTags path tag)))
           new of
      Identity (tags, rest) -> tags ++ rest
