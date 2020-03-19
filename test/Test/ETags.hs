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


newtype ArbETag = ArbETag { getArbETag :: Tag }
  deriving Show

instance Arbitrary ArbETag where
    arbitrary = fmap ArbETag $
          Tag
      <$> (TagName <$> genTextNonEmpty)
      <*> genTagKind
      <*> (TagFile  <$> genFilePath)
      <*> oneof
            [ TagLine . getPositive <$> arbitrary
            -- we are generating `TagLineCol` even though they are not present;
            -- The roundTrip property will check if the address was projected
            -- to `TagLine`.
            , TagLineCol <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)
            ]
      <*> listOf genField
    shrink = map ArbETag . shrinkTag . getArbETag


--
-- Properties
--

eTagsBuilderSizeProp :: ArbETag -> Bool
eTagsBuilderSizeProp (ArbETag ts) =
    case formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        BL.length (BB.toLazyByteString builder) == fromIntegral builderSize
