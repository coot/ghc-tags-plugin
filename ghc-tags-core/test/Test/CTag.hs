{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.CTag (tests) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           GhcTags.Tag
import qualified GhcTags.CTag as CTag

import           Test.Tag.Generators


tests :: TestTree
tests = testGroup "CTag"
  [ testGroup "CTag ByteString codec"
    [ testProperty "parseTag . formatTag" roundTripCTagProp
    , testProperty "parseHeader . formatHeader" roundTripHeaderProp
    ]
  , testGroup "TagKind to Char converstion"
    [ testProperty "tagKindToChar . charToTagKind" tagKindCharToCharProp
    , testProperty "charToTagKind . tagKindToChar" tagKindTagKindToTagKindProp
    ]
  ]

--
-- CTag generator
--

newtype ArbCTag = ArbCTag { getArbCTag :: CTag }
  deriving Show

instance Arbitrary ArbCTag where
    arbitrary = fmap ArbCTag $
          Tag
      <$> (TagName <$> genTextNonEmpty)
      <*> genTagKind SingCTag
      -- the 'roundTripProp' property holds only for normalised paths
      <*> genTagFilePath -- TODO normalise
      <*> frequency
            [ (2, TagLine . getPositive <$> arbitrary)
            -- we are generating `TagLineCol` even though they are not present
            -- in ctag files; The roundTrip property will check if the address
            -- was projected to `TagLine`.
            , (2, TagLineCol <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary))
            , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> pure NoTagDefinition
      <*> (TagFields <$> listOf genField)
    shrink = map ArbCTag . shrinkTag . getArbCTag


roundTripCTagProp :: ArbCTag -> Property
roundTripCTagProp (ArbCTag tag) =
    let bs   = BL.toStrict
             . BB.toLazyByteString
             . CTag.formatTag
             $ tag
        mtag = AT.parseOnly CTag.parseTag
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
    projectTagAddress :: CTag -> CTag
    projectTagAddress t@Tag {tagAddr = TagLineCol line _} =
      t { tagAddr = TagLine line }
    projectTagAddress t = t



--
-- Header generator
--

data ArbHeader = ArbHeader { getArgHeader :: CTag.Header }
  deriving Show

instance Arbitrary ArbHeader where
    arbitrary =
      ArbHeader
        <$> oneof
            [ CTag.Header CTag.FileEncoding
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.FileEncoding
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.FileFormat
                <$> genLanguageText
                <*> (getPositive <$> arbitrary)
                <*> genComment
            , CTag.Header CTag.FileSorted
                <$> genLanguageText
                <*> (getPositive <$> arbitrary)
                <*> genComment
            , CTag.Header CTag.OutputMode
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.KindDescription
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.KindSeparator
                <$> genLanguageText
                <*> genComment
                <*> genTextNonEmpty
            , CTag.Header CTag.ProgramAuthor
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.ProgramName
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.ProgramUrl
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.ProgramVersion
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.ExtraDescription
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header CTag.FieldDescription
                <$> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            , CTag.Header
                <$> (CTag.PseudoTag <$> genPseudoTagName)
                <*> genLanguageText
                <*> genTextNonEmpty
                <*> genComment
            ]

    shrink (ArbHeader CTag.Header { CTag.headerType, CTag.headerLanguage, CTag.headerArg, CTag.headerComment}) =
      [ ArbHeader $ CTag.Header headerType headerLanguage' headerArg headerComment
      | lang <- shrink headerLanguage
      , let headerLanguage' =
              lang >>= (\x -> if Text.null x then Nothing else Just x)
      ]
      ++ 
      [ ArbHeader $ CTag.Header headerType headerLanguage headerArg' headerComment
      | headerArg' <-
          case CTag.headerTypeSing headerType of
            CTag.SingHeaderTypeText -> filter (not . Text.null) (shrink headerArg)
            CTag.SingHeaderTypeInt  -> shrink headerArg
      ]
      ++
      [ ArbHeader $ CTag.Header headerType headerLanguage headerArg headerComment'
      | headerComment' <- fixText `map` shrink headerComment
      ]


genPseudoTagName :: Gen Text
genPseudoTagName =
    suchThat (Text.filter (/= '!') . fixText <$> arbitrary)
             (not . Text.null)

genLanguageText :: Gen (Maybe Text)
genLanguageText = oneof
  [ pure Nothing
  , Just <$> genTextNonEmpty
  ]

genComment :: Gen Text
genComment = fixText <$> arbitrary

roundTripHeaderProp :: ArbHeader -> Property
roundTripHeaderProp (ArbHeader h) =
    let bs = BL.toStrict
           . BB.toLazyByteString
           . CTag.formatHeader
           $ h
        mh = AT.parseOnly CTag.parseHeader
           . Text.decodeUtf8
           $ bs
    in case mh of
      Left  err  -> counterexample
                      ("parser error: " ++ err ++ " bs: " ++ (Text.unpack (Text.decodeUtf8 bs)))
                      (property False)
      Right h'   -> counterexample
                      (show $ Text.decodeUtf8 bs)
                      (h === h')


--
--
--

tagKindCharToCharProp :: Char -> Bool
tagKindCharToCharProp c = Just c == CTag.tagKindToChar (CTag.charToTagKind c)

newtype ArbCTagKind = ArbCTagKind { getArbCTagKind :: CTagKind }
  deriving Show

instance Arbitrary ArbCTagKind where
    arbitrary = ArbCTagKind <$> genTagKind SingCTag


tagKindTagKindToTagKindProp :: ArbCTagKind -> Bool
tagKindTagKindToTagKindProp (ArbCTagKind tk) =
      (case tk of
        NoKind -> Nothing
        _      -> Just tk)
    ==
      (CTag.charToTagKind <$> CTag.tagKindToChar tk)
