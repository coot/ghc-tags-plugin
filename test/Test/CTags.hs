{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.CTags (tests) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           Plugin.GhcTags.Tag
import qualified Plugin.GhcTags.CTags as CTags

import           Test.Tag.Generators


tests :: TestTree
tests = testGroup "CTags"
  [ testProperty "round-trip" roundTripProp
  ]

--
-- Generators
--

newtype ArbCTag = ArbCTag { getArbCTag :: Tag }
  deriving Show

instance Arbitrary ArbCTag where
    arbitrary = fmap ArbCTag $
          Tag
      <$> (TagName <$> genTextNonEmpty)
      <*> genTagKind
      <*> (TagFile  <$> genFilePath)
      <*> frequency
            [ (2, TagLine . getPositive <$> arbitrary)
            -- we are generating `TagLineCol` even though they are not present;
            -- The roundTrip property will check if the address was projected
            -- to `TagLine`.
            , (2, TagLineCol <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary))
            , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> listOf genField
    shrink = map ArbCTag . shrinkTag . getArbCTag


roundTripProp :: ArbCTag -> Property
roundTripProp (ArbCTag tag) =
    let bs   = BL.toStrict
             . BB.toLazyByteString
             . CTags.formatTag
             $ tag
        mtag = AT.parseOnly CTags.parseTag
             . Text.decodeUtf8
             $ bs
    in case mtag of
      Left  err  -> counterexample
                      ("parser error: " ++ err ++ " bs: " ++ (Text.unpack (Text.decodeUtf8 bs)))
                      (property False)
      Right tag' -> counterexample
                      (show $ Text.decodeUtf8 bs)
                      (projectTagAddress tag === tag')
  where
    projectTagAddress :: Tag -> Tag
    projectTagAddress t@Tag {tagAddr = TagLineCol line _} =
      t { tagAddr = TagLine line }
    projectTagAddress t = t
