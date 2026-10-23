{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ETag (tests) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (sortBy)
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
      <*> genTagKind SingETag
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



prepareETagsProp :: [ArbETag] -> Property
prepareETagsProp tags0 =
    prepareTags ETag.compareTags tags
    ===
    expected
  where
    tags = getArbETag <$> tags0

    sameIdentifier a b = on (==) tagName a b
                      && on (==) tagFilePath a b
    -- Model the preferred declaration independently of sorting/adjacency.
    redundant tag = any (\other -> sameIdentifier tag other && preferred tag other) tags

    preferred tag other
      | tagKind tag `elem` [TkTerm, TkFunction] = tagKind other == TkTypeSignature
      | tagKind tag == TkTypeConstructor =
          tagKind other `elem` [TkDataConstructor, TkGADTConstructor]
      | otherwise = False
    expected = sortBy ETag.compareTags (filter (not . redundant) tags)


--
-- Properties
--

eTagsBuilderSizeProp :: ArbETag -> Bool
eTagsBuilderSizeProp (ArbETag ts) =
    case formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        BL.length (BB.toLazyByteString builder) == fromIntegral builderSize
