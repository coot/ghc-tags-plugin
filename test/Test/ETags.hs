{-# LANGUAGE NamedFieldPuns  #-}

module Test.ETags (tests) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           Plugin.GhcTags.Tag
import           Plugin.GhcTags.ETags.Formatter

import           Test.Tag.Generators


tests :: TestTree
tests = testGroup "ETags"
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
      <*> (TagFile  <$> genFilePath)
      <*> (TagLineCol <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary))
      <*> (TagDefinition <$> genTextNonEmpty)
      <*> listOf genField
    shrink = map ArbETag . shrinkTag SingETag . getArbETag


--
-- Properties
--

eTagsBuilderSizeProp :: ArbETag -> Bool
eTagsBuilderSizeProp (ArbETag ts) =
    case formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        BL.length (BB.toLazyByteString builder) == fromIntegral builderSize
