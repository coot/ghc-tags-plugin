{-# LANGUAGE GADTs #-}

module GhcTags.CTag.Utils
  ( stripColumn
  , tagKindToChar
  , charToTagKind
  ) where

import           GhcTags.Tag


-- | Remove the column, which cannot be represented in ctags output.
stripColumn :: CTag -> CTag
stripColumn tag = case tagAddr tag of
    TagLineCol line _ -> tag { tagAddr = TagLine line }
    _                 -> tag


tagKindToChar :: TagKind -> Maybe Char
tagKindToChar tk = case tk of
    TkModule                  -> Just 'M'
    TkTerm                    -> Just '`'
    TkFunction                -> Just 'λ'
    TkTypeConstructor         -> Just 'Λ'
    TkDataConstructor         -> Just 'c'
    TkGADTConstructor         -> Just 'g'
    TkRecordField             -> Just 'r'
    TkTypeSynonym             -> Just '≡'
    TkTypeSignature           -> Just '⊢'
    TkPatternSynonym          -> Just 'p'
    TkTypeClass               -> Just 'C'
    TkTypeClassMember         -> Just 'm'
    TkTypeClassInstance       -> Just 'i'
    TkTypeClassInstanceMember -> Just 'x'
    TkTypeFamily              -> Just 'f'
    TkTypeFamilyInstance      -> Just 'F'
    TkDataTypeFamily          -> Just 'd'
    TkDataTypeFamilyInstance  -> Just 'D'
    TkForeignImport           -> Just 'I'
    TkForeignExport           -> Just 'E'

    CharKind c                -> Just c
    NoKind                    -> Nothing


charToTagKind :: Char -> TagKind
charToTagKind c = case c of
     'M' -> TkModule
     '`' -> TkTerm
     'λ' -> TkFunction
     'Λ' -> TkTypeConstructor
     'c' -> TkDataConstructor
     'g' -> TkGADTConstructor
     'r' -> TkRecordField
     '≡' -> TkTypeSynonym
     '⊢' -> TkTypeSignature
     'p' -> TkPatternSynonym
     'C' -> TkTypeClass
     'm' -> TkTypeClassMember
     'i' -> TkTypeClassInstance
     'x' -> TkTypeClassInstanceMember
     'f' -> TkTypeFamily
     'F' -> TkTypeFamilyInstance
     'd' -> TkDataTypeFamily
     'D' -> TkDataTypeFamilyInstance
     'I' -> TkForeignImport
     'E' -> TkForeignExport

     _   -> CharKind c
