{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tag.Generators where

import qualified Data.Char as Char
import           Data.List as List
import           Data.Maybe (isNothing)
import           Data.Text   (Text)
import qualified Data.Text as Text

import           Algebra.Lattice

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           Plugin.GhcTags.Tag

--
-- Generators
--

-- a quick hack
genTextNonEmpty :: Gen Text
genTextNonEmpty =
    suchThat
      (fixText <$> arbitrary)
      (not . Text.null)

-- filter only printable characters, removing tabs and newlines which have
-- special role in vim tag syntax
fixText :: Text -> Text
fixText = Text.filter ( ((/= Char.Control) . Char.generalCategory)
                        /\ Char.isPrint)

fixFilePath :: String -> String
fixFilePath = List.filter ( ((/= Char.Control) . Char.generalCategory)
                            /\ Char.isPrint)

genFilePath :: Gen String
genFilePath =
    suchThat
      (fixFilePath <$> arbitrary)
      (not . null)

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
fixFieldText = Text.filter ( (/= ':')
                             /\ ((/= Char.Control) . Char.generalCategory)
                             /\ Char.isPrint)


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

genTagKind :: SingTagKind tk -> Gen (TagKind tk)
genTagKind SingETag = pure NoKind
genTagKind SingCTag = oneof
    [ pure NoKind
    , CharKind <$> genChar
    , GhcKind <$> genGhcKind
    ]
  where
    genChar = suchThat arbitrary
                       ( ((/= Char.Control) . Char.generalCategory)
                         /\ (/= ':')
                         /\ (isNothing . charToGhcKind)
                       )

shrinkTag' :: SingTagKind tk -> Tag tk -> [Tag tk]
shrinkTag' _sing tag@Tag {tagName, tagAddr, tagFields} =
      [ tag { tagName = TagName x }
      | x <- fixText `map` shrink (getTagName tagName)
      , not (Text.null x)
      ]
   ++ [ tag { tagAddr = addr }
      | addr <- case tagAddr of
          TagLineCol line col ->
            [ TagLineCol line col'
            | col' <- shrink col
            ]
            ++
            [ TagLineCol line' col
            | line' <- shrink line
            ]
          TagLine  addr -> TagLine `map` shrink addr
          TagCommand (ExCommand addr) ->
              TagLine 0
            : (TagCommand . ExCommand . wrap '/' . fixAddr)
              `map` (shrink . stripEnds) addr
      ,  addr /= tagAddr -- wrap might restore the same address!
      ]
   ++ [ tag { tagFields = fields }
      | fields <- shrinkList (const []) tagFields
      ]
    where
      stripEnds :: Text -> Text
      stripEnds addr = case Text.uncons addr of
        Nothing -> error "impossible happend"
        Just (_, addr') -> case Text.unsnoc addr' of
          Nothing -> error "impossible happend"
          Just (addr'', _) -> addr''


shrinkTag :: SingTagKind tk -> Tag tk -> [Tag tk]
shrinkTag sing tag@Tag {tagFile} =
      shrinkTag' sing tag
   ++ [ tag { tagFile = TagFile x }
      | x <- fixFilePath `map` shrink (getTagFile tagFile)
      , not (null x)
      ]
