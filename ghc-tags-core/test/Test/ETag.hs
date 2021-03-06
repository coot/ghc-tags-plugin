{-# LANGUAGE NamedFieldPuns  #-}

module Test.ETag (tests) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           GhcTags.Tag
import           GhcTags.ETag.Formatter

import           Test.Tag.Generators


tests :: TestTree
tests = testGroup "ETag"
  [ testProperty "etag size" eTagsBuilderSizeProp
  ]


--
-- Generators
--


newtype ArbETag = ArbETag { getArbETag :: ETag }
  deriving Show

instance Arbitrary ArbETag where
    arbitrary = fmap ArbETag $
          Tag
      <$> (TagName <$> genTextNonEmpty)
      <*> genTagKind SingETag
      <*> genTagFilePath
      <*> oneof [ TagLine    <$> (getPositive <$> arbitrary)
                , TagLineCol <$> (getPositive <$> arbitrary)
                             <*> (getPositive <$> arbitrary)
                , return NoAddress
                ]
      <*> (TagDefinition <$> genTextNonEmpty)
      <*> pure NoTagFields
    shrink = map ArbETag . shrinkTag . getArbETag


--
-- Properties
--

eTagsBuilderSizeProp :: ArbETag -> Bool
eTagsBuilderSizeProp (ArbETag ts) =
    case formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        BL.length (BB.toLazyByteString builder) == fromIntegral builderSize
