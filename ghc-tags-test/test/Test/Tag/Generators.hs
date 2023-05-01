{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tag.Generators where

import qualified Data.Char as Char
import           Data.Text   (Text)
import qualified Data.Text as Text

import           Algebra.Lattice

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           GhcTags.Tag

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

fixFilePath :: TagFilePath -> TagFilePath
fixFilePath = TagFilePath
            . Text.filter ( ((/= Char.Control) . Char.generalCategory)
                            /\ Char.isPrint)
            . getRawFilePath

genTagFilePath :: Gen TagFilePath
genTagFilePath =
    suchThat
      (fixFilePath . TagFilePath . Text.pack <$> arbitrary)
      (not . Text.null . getRawFilePath)

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

genTagKind :: SingTagKind tk -> Gen TagKind
genTagKind SingETag = pure NoKind
genTagKind SingCTag = oneof
    [ pure TkModule
    , pure TkTerm
    , pure TkFunction
    , pure TkTypeConstructor
    , pure TkDataConstructor
    , pure TkGADTConstructor
    , pure TkRecordField
    , pure TkTypeSynonym
    , pure TkTypeSignature
    , pure TkPatternSynonym
    , pure TkTypeClass
    , pure TkTypeClassMember
    , pure TkTypeClassInstance
    , pure TkTypeClassInstanceMember
    , pure TkTypeFamily
    , pure TkTypeFamilyInstance
    , pure TkDataTypeFamily
    , pure TkDataTypeFamilyInstance
    , pure TkForeignImport
    , pure TkForeignExport
    , CharKind <$> genChar
    , pure NoKind
    ]
  where
    genChar = suchThat arbitrary
                       ( ((/= Char.Control) . Char.generalCategory)
                         /\ (/= ':')
                         /\ (not . flip elem ("`λΛcgr≡⊢pCmifFdDIExM" :: String))
                       )

shrinkTag' :: Tag tk -> [Tag tk]
shrinkTag' tag@Tag {tagName, tagAddr, tagFields} =
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
          NoAddress -> []
      ,  addr /= tagAddr -- wrap might restore the same address!
      ]
   ++ case tagFields of
        TagFields fields ->
          [ tag { tagFields = TagFields fields' }
          | fields' <- shrinkList (const []) fields
          ]
        NoTagFields ->
          []
    where
      stripEnds :: Text -> Text
      stripEnds addr = case Text.uncons addr of
        Nothing -> error "impossible happend"
        Just (_, addr') -> case Text.unsnoc addr' of
          Nothing -> error "impossible happend"
          Just (addr'', _) -> addr''


shrinkTag :: Tag tk -> [Tag tk]
shrinkTag tag@Tag {tagFilePath = TagFilePath tagFilePath} =
      shrinkTag' tag
   ++ [ tag { tagFilePath = tagFilePath' }
      | tagFilePath' <- (fixFilePath . TagFilePath . Text.pack) `map` shrink (Text.unpack tagFilePath)
      , not (Text.null $ getRawFilePath tagFilePath')
      ]
