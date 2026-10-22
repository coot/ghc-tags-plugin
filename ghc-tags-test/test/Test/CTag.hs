{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.CTag (tests) where

import qualified Data.Attoparsec.ByteString.Char8 as AChar
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BL
import           Data.List (sortBy)
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
  , testProperty "same-location signature filtering matches model" filterSameLocationSignaturesProp
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


newtype SignatureExample = SignatureExample
    { signatureInput    :: [CTag]
    }
  deriving Show

instance Arbitrary SignatureExample where
    arbitrary = do
      ArbCTag base <- arbitrary
      functions <- getNonEmpty <$> arbitrary
      Positive line <- arbitrary
      kind <- elements [TkTerm, TkFunction]
      let location = 1 + line `mod` 10000
          address n = case tagAddr base of
            TagLineCol _ col -> TagLineCol n col
            _ -> TagLine n
          signature = base { tagKind = TkTypeSignature
                           , tagFilePath = TagFilePath "A.hs"
                           , tagAddr = address (location + 1)
                           }
          -- All of these share the signature's name, file and position, but
          -- can carry distinct metadata and can include repeated copies.
          redundant =
            [ signature { tagKind = kind, tagFields = tagFields fnTag }
            | ArbCTag fnTag <- functions
            ]
          -- Keep other positions (eg local scopes, already parsed from disk),
          -- another file with the same name, and another name at this position.
          unrelated =
            [ signature { tagKind = kind, tagAddr = TagLine location }
            , signature { tagKind = kind, tagAddr = TagLine (location + 2) }
            , signature { tagKind = kind, tagFilePath = TagFilePath "B.hs" }
            , signature { tagKind = kind
                        , tagName = TagName (getTagName (tagName base) <> "_other")
                        }
            ]
      pure SignatureExample
        { signatureInput = sortBy CTag.compareTags (signature : redundant ++ redundant ++ unrelated)
        }

    shrink SignatureExample {signatureInput} =
      [ SignatureExample (sortBy CTag.compareTags smaller)
      | smaller <- shrinkList shrinkTag signatureInput
      ]


filterSameLocationSignaturesProp :: SignatureExample -> Property
filterSameLocationSignaturesProp SignatureExample {signatureInput} =
    let -- Independent model: inspect the entire input for a matching signature,
        -- without relying on sorted groups or adjacency.
        isRedundant tag =
          tagKind tag `elem` [TkTerm, TkFunction]
          && any (\sig -> tagKind sig == TkTypeSignature
                       && tagName sig == tagName tag
                       && tagFilePath sig == tagFilePath tag
                       && tagAddr sig == tagAddr tag)
                 signatureInput
        expected = filter (not . isRedundant) signatureInput
    in filterSameLocationTags signatureInput === expected


roundTripCTagProp :: ArbCTag -> Property
roundTripCTagProp (ArbCTag tag) =
    let bs   = BL.toStrict
             . BB.toLazyByteString
             . CTag.formatTag
             $ tag
        mtag = AChar.parseOnly CTag.parseTag
             $ bs
    in case mtag of
      Left  err  -> counterexample
                      ("parser error: " ++ err ++ " bs: " ++ BS.Char8.unpack bs)
                      (property False)
      Right tag' -> counterexample
                      (show $ Text.decodeUtf8 bs)
                      (projectTag tag === tag')
  where
    projectTag :: CTag -> CTag
    projectTag t@Tag {tagFilePath = TagFilePath path, tagAddr} =
      t { tagFilePath = TagFilePath
                      . Text.decodeUtf8
                      . rawFilePathToBS
                      . normaliseRawFilePath
                      . rawFilePathFromBS
                      . Text.encodeUtf8
                      $ path
        , tagAddr = case tagAddr of
            TagLineCol line _ -> TagLine line
            _                 -> tagAddr
        }



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
        mh = AChar.parseOnly CTag.parseHeader
           $ bs
    in case mh of
      Left  err  -> counterexample
                      ("parser error: " ++ err ++ " bs: " ++ BS.Char8.unpack bs)
                      (property False)
      Right h'   -> counterexample
                      (show $ Text.decodeUtf8 bs)
                      (h === h')

--
--
--

tagKindCharToCharProp :: Char -> Bool
tagKindCharToCharProp c = Just c == CTag.tagKindToChar (CTag.charToTagKind c)

newtype ArbCTagKind = ArbCTagKind { getArbCTagKind :: TagKind }
  deriving Show

instance Arbitrary ArbCTagKind where
    arbitrary = ArbCTagKind <$> genTagKind SingCTag


tagKindTagKindToTagKindProp :: ArbCTagKind -> Property
tagKindTagKindToTagKindProp (ArbCTagKind tk) =
      (case tk of
        NoKind -> Nothing
        _      -> Just tk)
    ===
      (CTag.charToTagKind <$> CTag.tagKindToChar tk)
