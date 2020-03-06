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


tests :: TestTree
tests = testGroup "Vim"
  [ testProperty "round-trip"
      (either (flip counterexample (property False)) property
      . roundTrip
      . getArbTag)
  ]


-- a quick hack
genTextNonEmpty :: Gen Text
genTextNonEmpty =
    suchThat
      (fixText <$> arbitrary)
      (not . Text.null)

fixText :: Text -> Text
fixText = Text.filter (\x -> x /= '\t' && x /= '\n')


genField :: Gen TagField
genField =
        TagField
    <$> suchThat g (not . Text.null)
    <*> g
  where 
    g :: Gen Text
    g = fixFieldText <$> arbitrary

fixFieldText :: Text -> Text
fixFieldText = Text.filter (\x -> x /= '\t' && x /= ':' && x /= '\n')

fixAddr :: Text -> Text
fixAddr = fixText . Text.replace ";\"" ""

genKind :: Gen TagKind
genKind = elements
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


newtype ArbTag = ArbTag { getArbTag :: Tag }
  deriving Show

instance Arbitrary ArbTag where
    arbitrary = fmap ArbTag $
          Tag
      <$> (TagName <$> genTextNonEmpty)
      <*> (TagFile <$> genTextNonEmpty)
      <*> oneof
            [ Left . getPositive <$> arbitrary
            , Right . (Text.cons '/' . flip Text.snoc '/' . fixAddr) <$> genTextNonEmpty
            ]
      <*> frequency
            [ (4, Just <$> genKind)
            -- TODO: fix parsing of this case
            -- , (1, pure Nothing)
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
                          : (Right . Text.cons '/' . flip Text.snoc '/' . fixAddr)
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
            

roundTrip :: Tag -> Either String Bool
roundTrip tag =
  let mtag = AT.parseOnly Vim.parseTag
           . Text.decodeUtf8
           . BL.toStrict
           . BB.toLazyByteString
           . Vim.formatTag
           $ tag
  in case mtag of
    Left  err  -> Left err
    Right tag' -> Right $ tag == tag'
