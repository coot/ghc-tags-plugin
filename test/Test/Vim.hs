{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Vim (tests) where

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
import qualified Plugin.GhcTags.Vim as Vim

import           Test.Tag.Generators


tests :: TestTree
tests = testGroup "Vim"
  [ testProperty "round-trip" (roundTrip . getArbTag)
  ]

--
-- Generators
--

newtype ArbTag = ArbTag { getArbTag :: Tag }
  deriving Show

instance Arbitrary ArbTag where
    arbitrary = fmap ArbTag $
          Tag
      <$> (TagName <$> genTextNonEmpty)
      <*> genTagKind
      <*> (TagFile  <$> genFilePath)
      <*> frequency
            [ (2, TagLine . getPositive <$> arbitrary)
            , (2, TagLineCol <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary))
            , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> listOf genField
    shrink = map ArbTag . shrinkTag . getArbTag


roundTrip :: Tag -> Property
roundTrip tag =
    let bs   = BL.toStrict
             . BB.toLazyByteString
             . Vim.formatTag
             $ tag
        mtag = AT.parseOnly Vim.parseTag
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
