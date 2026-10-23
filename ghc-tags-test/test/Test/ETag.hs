{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ETag (tests) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (isSubsequenceOf, sortBy)
-- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           GhcTags.Tag
import           GhcTags.ETag.Formatter
import qualified GhcTags.ETag as ETag

import           Test.Tag.Generators


tests :: TestTree
tests = testGroup "ETag"
  [ testProperty "etag size" eTagsBuilderSizeProp
  , testProperty "declaration preparation matches model" prepareETagsProp
  , testProperty "constructor filtering is independent of adjacency" prepareETagsAdjacencyProp
  , testProperty "preparation preserves input order for equal output keys" prepareETagsStableOrderProp
  ]


--
-- Generators
--


newtype ArbETag = ArbETag { getArbETag :: ETag }
  deriving Show

instance Arbitrary ArbETag where
    arbitrary = fmap ArbETag $
          Tag
      <$> frequency
            [ (3, TagName <$> elements ["alpha", "beta", "gamma"])
            , (1, TagName <$> genTextNonEmpty)
            ]
      -- Generated etags carry declaration kinds until formatting.
      <*> genTagKind SingCTag
      <*> frequency
            [ (3, TagFilePath <$> elements ["A.hs", "B.hs", "C.hs"])
            , (1, genTagFilePath)
            ]
      <*> oneof [ TagLine    <$> (getPositive <$> arbitrary)
                , TagLineCol <$> (getPositive <$> arbitrary)
                             <*> (getPositive <$> arbitrary)
                , return NoAddress
                ]
      <*> (TagDefinition <$> genTextNonEmpty)
      <*> pure NoTagFields
    shrink = map ArbETag . shrinkTag . getArbETag


newtype RelatedETags = RelatedETags [ETag]
  deriving Show

instance Arbitrary RelatedETags where
    arbitrary = do
      ArbETag base <- arbitrary
      tags <- listOf arbitrary
      pure $ RelatedETags
        [ tag { tagName = tagName base, tagFilePath = tagFilePath base }
        | ArbETag tag <- tags
        ]

    shrink (RelatedETags tags) = RelatedETags <$> shrinkList shrinkTag tags


prepareETagsAdjacencyProp :: RelatedETags -> Property
prepareETagsAdjacencyProp (RelatedETags tags) =
    checkCoverage
    $ cover 10 mixed "candidates, preferred declarations and unrelated kinds"
    $ candidates (prepareTags ETag.compareTags tags)
      ===
      candidates (prepareTags ETag.compareTags relevant)
  where
    preferredKinds = [TkDataConstructor, TkGADTConstructor]
    candidates = filter ((== TkTypeConstructor) . tagKind)
    isRelevant tag = tagKind tag `elem` (TkTypeConstructor : preferredKinds)
    relevant = filter isRelevant tags
    mixed = any ((== TkTypeConstructor) . tagKind) tags
         && any ((`elem` preferredKinds) . tagKind) tags
         && any (not . isRelevant) tags


newtype EqualKeyETags = EqualKeyETags [ETag]
  deriving Show

instance Arbitrary EqualKeyETags where
    arbitrary = do
      ArbETag base <- arbitrary
      Positive line <- arbitrary
      tags <- listOf $ do
        ArbETag tag <- arbitrary
        address <- oneof
          [ pure (TagLine line)
          , TagLineCol line . getNonNegative <$> arbitrary
          ]
        pure tag { tagName = tagName base
                 , tagFilePath = tagFilePath base
                 , tagAddr = address
                 }
      pure (EqualKeyETags tags)

    shrink (EqualKeyETags tags) =
      EqualKeyETags <$> shrinkList shrinkEqualKey tags
      where
        shrinkEqualKey tag =
          [ smaller
          | smaller <- shrinkTag tag
          , ETag.compareTags tag smaller == EQ
          ]


prepareETagsStableOrderProp :: EqualKeyETags -> Property
prepareETagsStableOrderProp (EqualKeyETags tags) =
    checkCoverage
    $ cover 10 (length prepared > 1) "multiple survivors with equal output keys"
    $ counterexample ("Prepared tags: " ++ show prepared)
    $ prepared `isSubsequenceOf` tags === True
  where
    prepared = prepareTags ETag.compareTags tags



prepareETagsProp :: [ArbETag] -> Property
prepareETagsProp tags0 =
    prepareTags ETag.compareTags tags
    ===
    expected
  where
    tags = getArbETag <$> tags0

    sameIdentifier a b = on (==) tagName a b
                      && on (==) tagFilePath a b
    -- Signatures are reconciled in AST binding groups before preparation.
    -- Model constructor preference independently of sorting/adjacency.
    redundant tag = tagKind tag == TkTypeConstructor
                 && any (\other -> sameIdentifier tag other
                                && tagKind other `elem` [TkDataConstructor, TkGADTConstructor]) tags
    expected = sortBy ETag.compareTags (filter (not . redundant) tags)


--
-- Properties
--

eTagsBuilderSizeProp :: ArbETag -> Bool
eTagsBuilderSizeProp (ArbETag ts) =
    case formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        BL.length (BB.toLazyByteString builder) == fromIntegral builderSize
