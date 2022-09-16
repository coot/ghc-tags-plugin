{-# LANGUAGE GADTs #-}

module GhcTags.CTag.Utils
  ( tagKindToChar
  , charToTagKind
  ) where

import           GhcTags.Tag

tagKindToChar :: TagKind -> Maybe Char
tagKindToChar tk = case tk of
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
     'f' -> TkTypeFamily
     'F' -> TkTypeFamilyInstance
     'd' -> TkDataTypeFamily
     'D' -> TkDataTypeFamilyInstance
     'I' -> TkForeignImport
     'E' -> TkForeignExport

     _   -> CharKind c
