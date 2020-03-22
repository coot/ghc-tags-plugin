module GhcTags.CTags.Utils
  ( ghcKindToChar
  , charToGhcKind
  ) where

import           GhcTags.Ghc

ghcKindToChar :: GhcKind -> Char
ghcKindToChar tagKind = case tagKind of
    TkTerm                    -> '`'
    TkFunction                -> 'λ'
    TkTypeConstructor         -> 'Λ'
    TkDataConstructor         -> 'c'
    TkGADTConstructor         -> 'g'
    TkRecordField             -> 'r'
    TkTypeSynonym             -> '≡'
    TkTypeSignature           -> '⊢'
    TkPatternSynonym          -> 'p'
    TkTypeClass               -> 'C'
    TkTypeClassMember         -> 'm'
    TkTypeClassInstance       -> 'i'
    TkTypeFamily              -> 'f'
    TkTypeFamilyInstance      -> 'F'
    TkDataTypeFamily          -> 'd'
    TkDataTypeFamilyInstance  -> 'D'
    TkForeignImport           -> 'I'
    TkForeignExport           -> 'E'


charToGhcKind :: Char -> Maybe GhcKind
charToGhcKind c = case c of
     '`' -> Just TkTerm
     'λ' -> Just TkFunction
     'Λ' -> Just TkTypeConstructor
     'c' -> Just TkDataConstructor
     'g' -> Just TkGADTConstructor
     'r' -> Just TkRecordField
     '≡' -> Just TkTypeSynonym
     '⊢' -> Just TkTypeSignature
     'p' -> Just TkPatternSynonym
     'C' -> Just TkTypeClass
     'm' -> Just TkTypeClassMember
     'i' -> Just TkTypeClassInstance
     'f' -> Just TkTypeFamily
     'F' -> Just TkTypeFamilyInstance
     'd' -> Just TkDataTypeFamily
     'D' -> Just TkDataTypeFamilyInstance
     'I' -> Just TkForeignImport
     'E' -> Just TkForeignExport
     _   -> Nothing
