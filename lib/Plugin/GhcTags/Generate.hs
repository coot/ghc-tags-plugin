{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface for generatating tags for a parsed module.
--
module Plugin.GhcTags.Generate
  ( GhcTag (..)
  , GhcTags
  , TagKind (..)
  , TagField (..)
  , tagKindToChar
  , charToTagKind
  , getGhcTags
  ) where

import           Data.List     (find)
import           Data.Maybe    (mapMaybe, maybeToList)
import           Data.Foldable (foldl')
import           Data.Text   (Text)
import qualified Data.Text as Text

-- Ghc imports
import           BasicTypes   ( SourceText (..)
                              )
import           FastString   ( FastString (..)
                              )
import           HsBinds      ( HsBindLR (..)
                              , PatSynBind (..)
                              , Sig (..)
                              )
import           HsDecls      ( ForeignImport (..)
                              , ClsInstDecl (..)
                              , ConDecl (..)
                              , DataFamInstDecl (..)
                              , FamEqn (..)
                              , FamilyDecl (..)
                              , FamilyInfo (..)
                              , ForeignDecl (..)
                              , LHsDecl
                              , HsConDeclDetails
                              , HsDecl (..)
                              , HsDataDefn (..)
                              , InstDecl (..)
                              , TyClDecl (..)
                              , TyFamInstDecl (..)
                              )
import           HsImpExp     ( IE (..)
                              , ieWrappedName
                              )
import           HsSyn        ( FieldOcc (..)
                              , GhcPs
                              , HsModule (..)
                              , IdP
                              , LFieldOcc
                              )
import           HsTypes      ( ConDeclField (..)
                              , HsConDetails (..)
                              , HsImplicitBndrs (..)
                              , HsType (..)
                              , LConDeclField
                              , LHsType
                              )
import           SrcLoc       ( GenLocated (..)
                              , Located
                              , SrcSpan (..)
                              , unLoc
                              )
import           RdrName      ( RdrName (..)
                              )
import           Name         ( nameOccName
                              , occNameFS
                              )


-- | `ctags` can generate tags kind, so do we.
--
data TagKind = TkTerm
             | TkFunction
             | TkTypeConstructor
             | TkDataConstructor
             | TkGADTConstructor
             | TkRecordField
             | TkTypeSynonym
             | TkTypeSignature
             | TkPatternSynonym
             | TkTypeClass
             | TkTypeClassMember
             | TkTypeClassInstance
             | TkTypeFamily
             | TkTypeFamilyInstance
             | TkDataTypeFamily
             | TkDataTypeFamilyInstance
             | TkForeignImport
             | TkForeignExport
  deriving (Ord, Eq, Show)


tagKindToChar :: TagKind -> Char
tagKindToChar tagKind = case tagKind of
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


charToTagKind :: Char -> Maybe TagKind
charToTagKind c = case c of
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



data TagField = TagField {
      fieldName  :: Text,
      fieldValue :: Text
    }
  deriving (Eq, Ord, Show)

fileField :: TagField
fileField = TagField { fieldName = "file", fieldValue = "" }


-- | We can read names from using fields of type 'GHC.Hs.Extensions.IdP' (a tpye
-- family) which for @'Parsed@ resolved to 'RdrName'
--
data GhcTag = GhcTag {
    gtSrcSpan  :: !SrcSpan
  , gtTag      :: !FastString
  , gtKind     :: !TagKind
  , gtFields   :: ![TagField]
  }
  deriving Show

appendField :: TagField -> GhcTag -> GhcTag
appendField f gt = gt { gtFields = f : gtFields gt }


type GhcTags = [GhcTag]


getFileTagField :: Maybe [IE GhcPs] -> Located RdrName -> Maybe TagField
getFileTagField Nothing   _name = Nothing
getFileTagField (Just ies) name =
      maybe (Just fileField) (const Nothing) $ find (\a -> ieName a == Just (unLoc name)) ies
  where
    -- TODO: the GHC's one is partial, and I got a panic error.
    ieName :: IE GhcPs -> Maybe (IdP GhcPs)
    ieName (IEVar _ (L _ n))              = Just $ ieWrappedName n
    ieName (IEThingAbs  _ (L _ n))        = Just $ ieWrappedName n
    ieName (IEThingWith _ (L _ n) _ _ _)  = Just $ ieWrappedName n
    ieName (IEThingAll  _ (L _ n))        = Just $ ieWrappedName n
    ieName _ = Nothing

mkGhcTag :: Located RdrName
         -- ^ @RdrName ~ IdP GhcPs@ it *must* be a name of a top level identifier.
         -> TagKind
         -- ^ tag's kind
         -> [TagField]
         -- ^ tag's fields
         -> GhcTag
mkGhcTag (L gtSrcSpan rdrName) gtKind gtFields =
    case rdrName of
      Unqual occName ->
        GhcTag { gtTag = occNameFS occName
               , gtSrcSpan
               , gtKind
               , gtFields
               }

      Qual _ occName ->
        GhcTag { gtTag = occNameFS occName
               , gtSrcSpan
               , gtKind
               , gtFields
               }

      -- Orig is the only one we are interested in
      Orig _ occName ->
        GhcTag { gtTag = occNameFS occName
               , gtSrcSpan
               , gtKind
               , gtFields
               }

      Exact eName -> 
        GhcTag { gtTag = occNameFS $ nameOccName eName
               , gtSrcSpan
               , gtKind
               , gtFields
               }


-- | Generate tags for a module - simple walk over the syntax tree.
--
-- Supported identifiers:
--  * top level terms
--  * data types
--  * record fields
--  * type synonyms
--  * type classes
--  * type class members
--  * type class instances
--  * type families
--  * type family instances
--  * data type families
--  * data type families instances
--  * data type family instances constructors
--
getGhcTags :: Located (HsModule GhcPs)
                      -> GhcTags
getGhcTags (L _ HsModule { hsmodDecls, hsmodExports }) = 
    reverse $ foldl' go [] hsmodDecls
  where
    mies :: Maybe [IE GhcPs]
    mies = map unLoc . unLoc <$> hsmodExports

    -- like 'mkGhcTag' but checks if the identifier is exported
    mkGhcTag' :: Located RdrName
              -- ^ @RdrName ~ IdP GhcPs@ it *must* be a name of a top level identifier.
              -> TagKind
              -- ^ tag's kind
              -> GhcTag
    mkGhcTag' a k = mkGhcTag a k (maybeToList $ getFileTagField mies a)

    go :: GhcTags -> LHsDecl GhcPs -> GhcTags
    go tags (L _ hsDecl) = case hsDecl of
      -- type or class declaration
      TyClD _ tyClDecl ->
        case tyClDecl of
          FamDecl { tcdFam } ->
            case mkFamilyDeclTags tcdFam of
              Just tag -> tag : tags
              Nothing  ->       tags

          SynDecl { tcdLName } ->
            mkGhcTag' tcdLName TkTypeSynonym : tags

          DataDecl { tcdLName, tcdDataDefn } -> 
            case tcdDataDefn of
              HsDataDefn { dd_cons } ->
                     mkGhcTag' tcdLName TkTypeConstructor
                   : (mkConsTags . unLoc) `concatMap` dd_cons
                  ++ tags

              XHsDataDefn {} -> tags

          -- TODO: add 'tcdATDefs'
          ClassDecl { tcdLName, tcdSigs, tcdMeths, tcdATs } ->
              -- class name
              mkGhcTag' tcdLName TkTypeClass
               -- class methods
             : (mkSigTags . unLoc) `concatMap` tcdSigs
               -- default methods
            ++ foldl' (\tags' hsBind -> mkHsBindLRTags (unLoc hsBind) ++ tags')
                     tags
                     tcdMeths
            -- associated types
            ++ (mkFamilyDeclTags . unLoc) `mapMaybe` tcdATs

          XTyClDecl {} -> tags

      InstD _ instDecl ->
        case instDecl of
          ClsInstD { cid_inst } ->
            case cid_inst of
              XClsInstDecl {} -> tags

              ClsInstDecl { cid_poly_ty, cid_tyfam_insts, cid_datafam_insts } ->
                  case cid_poly_ty of
                    XHsImplicitBndrs {} ->
                      tyFamTags ++ dataFamTags ++ tags

                    -- TODO: @hsbib_body :: LHsType GhcPs@
                    HsIB { hsib_body } ->
                      case mkLHsTypeTag hsib_body of
                        Nothing  -> tyFamTags ++ dataFamTags ++ tags
                        Just tag -> tag : tyFamTags ++ dataFamTags ++ tags
                where
                  dataFamTags = (mkDataFamInstDeclTag . unLoc) `concatMap` cid_datafam_insts
                  tyFamTags   = (mkTyFamInstDeclTag   . unLoc) `mapMaybe`  cid_tyfam_insts


          DataFamInstD { dfid_inst } ->
            mkDataFamInstDeclTag  dfid_inst ++ tags

          TyFamInstD { tfid_inst } ->
            case mkTyFamInstDeclTag tfid_inst of
              Nothing  -> tags
              Just tag -> tag : tags

          XInstDecl {} -> tags

      -- deriveving declaration
      DerivD {} -> tags

      -- value declaration
      ValD _ hsBind  -> mkHsBindLRTags hsBind ++ tags

      -- signature declaration
      SigD _ sig -> mkSigTags sig ++ tags

      -- default declaration
      DefD {} -> tags

      -- foreign declaration
      ForD _ foreignDecl ->
        case foreignDecl of
          ForeignImport { fd_name, fd_fi = CImport _ _ _mheader _ (L _ sourceText) } ->
                case sourceText of
                  NoSourceText -> tag
                  -- TODO: add header information from '_mheader'
                  SourceText s -> TagField "ffi" (Text.pack s) `appendField` tag
              : tags
            where
              tag = mkGhcTag' fd_name TkForeignImport

          ForeignExport { fd_name } ->
              mkGhcTag' fd_name TkForeignExport
            : tags

          XForeignDecl {} -> tags

      WarningD {}   -> tags
      AnnD {}       -> tags

      -- TODO: Rules are named it would be nice to get them too
      RuleD {}      -> tags

      -- TODO: splices
      SpliceD {}    -> tags

      DocD {}       -> tags
      RoleAnnotD {} -> tags
      XHsDecl {}    -> tags

    -- tags of all constructors of a type
    mkConsTags :: ConDecl GhcPs -> GhcTags
    mkConsTags ConDeclGADT { con_names, con_args } =
         flip mkGhcTag' TkGADTConstructor `map` con_names
      ++ mkHsConDeclDetails con_args
    mkConsTags ConDeclH98  { con_name, con_args } =
        mkGhcTag' con_name TkDataConstructor
      : mkHsConDeclDetails con_args
    mkConsTags XConDecl {} = []

    mkHsConDeclDetails :: HsConDeclDetails GhcPs -> GhcTags
    mkHsConDeclDetails (RecCon (L _ fields)) = foldl' f [] fields
      where
        f :: GhcTags -> LConDeclField GhcPs -> GhcTags
        f ts (L _ ConDeclField { cd_fld_names }) = foldl' g ts cd_fld_names
        f ts _ = ts

        g :: GhcTags -> LFieldOcc GhcPs -> GhcTags
        g ts (L _ FieldOcc { rdrNameFieldOcc }) =
            mkGhcTag' rdrNameFieldOcc TkRecordField
          : ts
        g ts _ = ts

    mkHsConDeclDetails _  = []

    mkHsBindLRTags :: HsBindLR GhcPs GhcPs -> GhcTags
    mkHsBindLRTags hsBind =
      case hsBind of
        FunBind { fun_id } -> [mkGhcTag' fun_id TkFunction]

        -- TODO
        -- This is useful fo generating tags for
        -- ````
        -- Just x = lhs
        -- ```
        PatBind {} -> []

        VarBind { var_id, var_rhs = L srcSpan _ } -> [mkGhcTag' (L srcSpan var_id) TkTerm]

        -- abstraction binding are only used after translaction
        AbsBinds {} -> []

        PatSynBind _ PSB { psb_id } -> [mkGhcTag' psb_id TkPatternSynonym]
        PatSynBind _ XPatSynBind {} -> []

        XHsBindsLR {} -> []

    mkSigTags :: Sig GhcPs -> GhcTags
    mkSigTags (TypeSig   _ lhs _)    = flip mkGhcTag' TkTypeSignature   `map` lhs
    mkSigTags (PatSynSig _ lhs _)    = flip mkGhcTag' TkPatternSynonym  `map` lhs
    mkSigTags (ClassOpSig _ _ lhs _) = flip mkGhcTag' TkTypeClassMember `map` lhs
    mkSigTags IdSig {}               = []
    -- TODO: generate theses with additional info (fixity)
    mkSigTags FixSig {}              = []
    mkSigTags InlineSig {}           = []
    -- SPECIALISE pragmas
    mkSigTags SpecSig {}             = []
    mkSigTags SpecInstSig {}         = []
    -- MINIMAL pragma
    mkSigTags MinimalSig {}          = []
    -- SSC pragma
    mkSigTags SCCFunSig {}           = []
    -- COMPLETE pragma
    mkSigTags CompleteMatchSig {}    = []
    mkSigTags XSig {}                = []

    mkFamilyDeclTags :: FamilyDecl GhcPs
                     -> Maybe GhcTag
    mkFamilyDeclTags FamilyDecl { fdLName, fdInfo } = Just $ mkGhcTag' fdLName tk
      where
        tk = case fdInfo of
              DataFamily           -> TkDataTypeFamily
              OpenTypeFamily       -> TkTypeFamily
              ClosedTypeFamily {}  -> TkTypeFamily
    mkFamilyDeclTags XFamilyDecl {} = Nothing

    -- used to generate tag of an instance declaration
    mkLHsTypeTag :: LHsType GhcPs -> Maybe GhcTag
    mkLHsTypeTag (L _ hsType) =
      case hsType of
        HsForAllTy {hst_body} -> mkLHsTypeTag hst_body
        
        HsQualTy {hst_body}   -> mkLHsTypeTag hst_body

        HsTyVar _ _ a         -> Just $ mkGhcTag a TkTypeClassInstance []

        HsAppTy _ a _         -> mkLHsTypeTag a
        HsOpTy _ _ a _        -> Just $ mkGhcTag a TkTypeClassInstance []
        HsKindSig _ a _       -> mkLHsTypeTag a

        _                     -> Nothing

    -- todo: type constructors
    mkDataFamInstDeclTag :: DataFamInstDecl GhcPs -> GhcTags
    mkDataFamInstDeclTag DataFamInstDecl { dfid_eqn } =
      case dfid_eqn of
        XHsImplicitBndrs {} -> []

        HsIB { hsib_body = FamEqn { feqn_tycon, feqn_rhs } } ->
          case feqn_rhs of
            HsDataDefn { dd_cons } ->
              mkGhcTag' feqn_tycon TkDataTypeFamilyInstance : (mkConsTags . unLoc) `concatMap` dd_cons
            XHsDataDefn {}         ->
              mkGhcTag' feqn_tycon TkDataTypeFamilyInstance : []

        HsIB { hsib_body = XFamEqn {} } -> []

    mkTyFamInstDeclTag :: TyFamInstDecl GhcPs -> Maybe GhcTag
    mkTyFamInstDeclTag TyFamInstDecl { tfid_eqn } =
      case tfid_eqn of
        XHsImplicitBndrs {} -> Nothing

        -- TODO: should we check @feqn_rhs :: LHsType GhcPs@ as well?
        HsIB { hsib_body = FamEqn { feqn_tycon } } ->
          Just $ mkGhcTag' feqn_tycon TkTypeFamilyInstance

        HsIB { hsib_body = XFamEqn {} } -> Nothing
