{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Vim (tests) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Text   (Text)
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
            [ (2, Left . getPositive <$> arbitrary)
            , (1, Right . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, Right . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> listOf genField
    shrink (ArbTag tag@Tag {tagName, tagFile, tagAddr, tagFields}) =
          [ ArbTag $ tag { tagName = TagName x }
          | x <- fixText `map` shrink (getTagName tagName)
          , not (Text.null x)
          ]
       ++ [ ArbTag $ tag { tagFile = TagFile x }
          | x <- fixFilePath `map` shrink (getTagFile tagFile)
          , not (null x)
          ]
       ++ [ ArbTag $ tag { tagAddr = addr }
          | addr <- case tagAddr of
              Left  addr -> Left `map` shrink addr
              Right addr -> Left 0
                          : (Right . wrap '/' . fixAddr)
                            `map` (shrink . stripEnds) addr
          ]
       ++ [ ArbTag $ tag { tagFields = fields }
          | fields <- shrinkList (const []) tagFields
          ]
        where
          stripEnds :: Text -> Text
          stripEnds addr = case Text.uncons addr of
            Nothing -> error "impossible happend"
            Just (_, addr') -> case Text.unsnoc addr' of
              Nothing -> error "impossible happend"
              Just (addr'', _) -> addr''


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
                    (tag === tag')
