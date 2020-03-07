{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Vim (tests) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import           Data.Maybe (isNothing)
import           Data.Text   (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           Plugin.GhcTags.Tag
import qualified Plugin.GhcTags.Vim as Vim


tests :: TestTree
tests = testGroup "Vim"
  [ testProperty "round-trip" (roundTrip . getArbTag)
  ]


-- a quick hack
genTextNonEmpty :: Gen Text
genTextNonEmpty =
    suchThat
      (fixText <$> arbitrary)
      (not . Text.null)

-- filter only printable characters, removing tabs and newlines which have
-- special role in vim tag syntax
fixText :: Text -> Text
fixText = Text.filter (\x -> x /= '\t' && x /= '\n' && Char.isPrint x)


genField :: Gen TagField
genField =
        TagField
    <$> suchThat g (not . Text.null)
    <*> g
  where
    g :: Gen Text
    g = fixFieldText <$> arbitrary

-- filter only printable characters, removing tabs, newlines and colons which
-- have special role in vim field syntax
fixFieldText :: Text -> Text
fixFieldText = Text.filter (\x -> x /= '\t' && x /= ':' && x /= '\n' && Char.isPrint x)


-- address cannot contain ";\"" sequence
fixAddr :: Text -> Text
fixAddr = fixText . Text.replace ";\"" ""

wrap :: Char -> Text -> Text
wrap c = Text.cons c . flip Text.snoc c

genGhcKind :: Gen GhcKind
genGhcKind = elements
  [ TkTerm
  , TkFunction
  , TkTypeConstructor
  , TkDataConstructor
  , TkGADTConstructor
  , TkRecordField
  , TkTypeSynonym
  , TkTypeSignature
  , TkPatternSynonym
  , TkTypeClass
  , TkTypeClassMember
  , TkTypeClassInstance
  , TkTypeFamily
  , TkTypeFamilyInstance
  , TkDataTypeFamily
  , TkDataTypeFamilyInstance
  , TkForeignImport
  , TkForeignExport
  ]

genTagKind :: Gen TagKind
genTagKind = oneof
    [ pure NoKind
    , CharKind <$> genChar
    , GhcKind <$> genGhcKind
    ]
  where
    genChar = suchThat arbitrary
                       (\x ->    x /= '\t'
                              && x /= '\n'
                              && x /= ':'
                              && x /= '\NUL'
                              && isNothing (charToGhcKind x)
                       )


newtype ArbTag = ArbTag { getArbTag :: Tag }
  deriving Show

instance Arbitrary ArbTag where
    arbitrary = fmap ArbTag $
          Tag
      <$> (TagName <$> genTextNonEmpty)
      <*> genTagKind
      <*> (TagFile <$> genTextNonEmpty)
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
          | x <- fixText `map` shrink (getTagFile tagFile)
          , not (Text.null x)
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
