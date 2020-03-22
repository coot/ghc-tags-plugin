{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface for generating tags for a parsed module.
--
module GhcTags.Ghc
  ( GhcTag (..)
  , GhcTags
  , GhcKind (..)
  , TagField (..)
  , ghcKindToChar
  , charToGhcKind
  , getGhcTags
  ) where


import           Data.Maybe    (mapMaybe, maybeToList)
import           Data.Foldable (foldl')
import           Data.Text   (Text)
import qualified Data.Text as Text

-- Ghc imports
import           BasicTypes   ( SourceText (..)
                              )
import           FastString   ( FastString (..)
                              )
import           FieldLabel   ( FieldLbl (..)
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
                              , IEWildcard (..)
                              , ieWrappedName
                              )
import           HsSyn        ( FieldOcc (..)
                              , GhcPs
                              , HsModule (..)
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
                              , rdrNameOcc
                              )
import           Name         ( nameOccName
                              , occNameFS
                              )


-- | `ctags` can generate tags kinds, so do we.
--
data GhcKind = TkTerm
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



-- | Unit of data associated with a tag.  Vim natively supports `file:` and
-- `kind:` tags but it can display any other tags too.
--
data TagField = TagField {
      fieldName  :: Text,
      fieldValue :: Text
    }
  deriving (Eq, Ord, Show)


-- | File field; tags which contain 'fileFields' are called static (aka static
-- in 'C'), such tags are only visible in the current file)
--
fileField :: TagField
fileField = TagField { fieldName = "file", fieldValue = "" }


-- | We can read names from using fields of type 'GHC.Hs.Extensions.IdP' (a type
-- family) which for @'Parsed@ resolved to 'RdrName'
--
data GhcTag = GhcTag {
    gtSrcSpan  :: !SrcSpan
  , gtTag      :: !FastString
  , gtKind     :: !GhcKind
  , gtFields   :: ![TagField]
  }
  deriving Show


appendField :: TagField -> GhcTag -> GhcTag
appendField f gt = gt { gtFields = f : gtFields gt }


type GhcTags = [GhcTag]


-- | Check if an identifier is exported, if it is not return 'fileField'.
--
getFileTagField :: Maybe [IE GhcPs] -> Located RdrName -> Maybe TagField
getFileTagField Nothing   _name = Nothing
getFileTagField (Just ies) (L _ name) =
    if any (\ie -> ieName ie == Just name) ies
      then Nothing
      else Just fileField
  where
    -- TODO: the GHC's one is partial, and I got a panic error.
    ieName :: IE GhcPs -> Maybe RdrName
    ieName (IEVar _ (L _ n))              = Just $ ieWrappedName n
    ieName (IEThingAbs  _ (L _ n))        = Just $ ieWrappedName n
    ieName (IEThingWith _ (L _ n) _ _ _)  = Just $ ieWrappedName n
    ieName (IEThingAll  _ (L _ n))        = Just $ ieWrappedName n
    ieName _ = Nothing


-- | Either class members or type constructors.
--
getFileTagFieldForMember :: Maybe [IE GhcPs]
                         -> Located RdrName -- member name / constructor name
                         -> Located RdrName -- type class name / type constructor name
                         -> Maybe TagField
getFileTagFieldForMember Nothing    _memberName _className = Nothing
getFileTagFieldForMember (Just ies) memberName  className  =
    if any go ies
      then Nothing
      else Just fileField
  where
    go :: IE GhcPs -> Bool

    go (IEVar _ (L _ n)) = ieWrappedName n == unLoc memberName

    go (IEThingAbs _ _)  = False

    go (IEThingAll _ (L _ n)) = ieWrappedName n == unLoc className

    go (IEThingWith _ _ IEWildcard{} _ _) = True

    go (IEThingWith _ (L _ n) NoIEWildcard ns lfls) =
            ieWrappedName n == unLoc className
         && (isInWrappedNames || isInFieldLbls)
      where
        -- the 'NameSpace' does not agree between things that are in the 'IE'
        -- list and passed member or type class names (constructor / type
        -- constructor names, respectively)
        isInWrappedNames = any ((== occNameFS (rdrNameOcc (unLoc memberName))) . occNameFS . rdrNameOcc . ieWrappedName . unLoc) ns
        isInFieldLbls    = any ((== occNameFS (rdrNameOcc (unLoc memberName))) . occNameFS . rdrNameOcc . flSelector. unLoc) lfls 

    go _ = False


-- | Create a 'GhcTag', effectively a smart constructor.
--
mkGhcTag :: Located RdrName
         -- ^ @RdrName ~ IdP GhcPs@ it *must* be a name of a top level identifier.
         -> GhcKind
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
getGhcTags ::Located (HsModule GhcPs)
           -> GhcTags
getGhcTags (L _ HsModule { hsmodDecls, hsmodExports }) = 
    reverse $ foldl' go [] hsmodDecls
  where
    mies :: Maybe [IE GhcPs]
    mies = map unLoc . unLoc <$> hsmodExports

    -- like 'mkGhcTag' but checks if the identifier is exported
    mkGhcTag' :: Located RdrName
              -- ^ @RdrName ~ IdP GhcPs@ it *must* be a name of a top level identifier.
              -> GhcKind
              -- ^ tag's kind
              -> GhcTag
    mkGhcTag' a k = mkGhcTag a k (maybeToList $ getFileTagField mies a)

    mkGhcTagForMember :: Located RdrName -- member name
                      -> Located RdrName -- class name
                      -> GhcKind
                      -> GhcTag
    mkGhcTagForMember memberName className kind =
      mkGhcTag memberName kind
        (maybeToList $ getFileTagFieldForMember mies memberName className)


    -- Main routine which traverse all top level declarations.
    --
    go :: GhcTags -> LHsDecl GhcPs -> GhcTags
    go tags (L _ hsDecl) = case hsDecl of

      -- type or class declaration
      TyClD _ tyClDecl ->
        case tyClDecl of

          -- type family declarations
          FamDecl { tcdFam } ->
            case mkFamilyDeclTags tcdFam Nothing of
              Just tag -> tag : tags
              Nothing  ->       tags

          -- type synonyms
          SynDecl { tcdLName } ->
            mkGhcTag' tcdLName TkTypeSynonym : tags

          -- data declaration:
          --   type,
          --   constructors,
          --   record fields
          --
          DataDecl { tcdLName, tcdDataDefn } -> 
            case tcdDataDefn of
              HsDataDefn { dd_cons } ->
                     mkGhcTag' tcdLName TkTypeConstructor
                   : (mkConsTags tcdLName . unLoc) `concatMap` dd_cons
                  ++ tags

              XHsDataDefn {} -> tags

          -- Type class declaration:
          --   type class name,
          --   type class members,
          --   default methods,
          --   default data type instance
          --
          ClassDecl { tcdLName, tcdSigs, tcdMeths, tcdATs, tcdATDefs } ->
              -- class name
              mkGhcTag' tcdLName TkTypeClass
               -- class methods
             : (mkClsMemberTags tcdLName . unLoc) `concatMap` tcdSigs
               -- default methods
            ++ foldl' (\tags' hsBind -> mkHsBindLRTags (unLoc hsBind) ++ tags')
                     []
                     tcdMeths
            -- associated types
            ++ (flip mkFamilyDeclTags (Just tcdLName) . unLoc) `mapMaybe` tcdATs
            -- associated type defaults (data type families, type families
            -- (open or closed)
            ++ foldl'
                (\tags' (L _ tyFamDeflEqn) ->
                  case tyFamDeflEqn of
                    FamEqn { feqn_rhs } -> 
                      case hsTypeTagName (unLoc feqn_rhs) of
                        -- TODO: add a `default` field
                        Just a  -> mkGhcTag' a TkTypeFamilyInstance : tags'
                        Nothing -> tags'
                    XFamEqn {} -> tags')
                [] tcdATDefs
            ++ tags

          XTyClDecl {} -> tags

      -- Instance declarations
      --  class instances
      --  type family instance
      --  data type family instances
      --
      InstD _ instDecl ->
        case instDecl of
          -- class instance declaration
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
                        Nothing  ->       tyFamTags ++ dataFamTags ++ tags
                        Just tag -> tag : tyFamTags ++ dataFamTags ++ tags
                where
                  -- associated type and data type family instances
                  dataFamTags = (mkDataFamInstDeclTag . unLoc) `concatMap` cid_datafam_insts
                  tyFamTags   = (mkTyFamInstDeclTag   . unLoc) `mapMaybe`  cid_tyfam_insts

          -- data family instance
          DataFamInstD { dfid_inst } ->
            mkDataFamInstDeclTag  dfid_inst ++ tags

          -- type family instance
          TyFamInstD { tfid_inst } ->
            case mkTyFamInstDeclTag tfid_inst of
              Nothing  ->       tags
              Just tag -> tag : tags

          XInstDecl {} -> tags

      -- deriving declaration
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
      SpliceD {}    -> tags
      DocD {}       -> tags
      RoleAnnotD {} -> tags
      XHsDecl {}    -> tags


    -- generate tags of all constructors of a type
    --
    mkConsTags :: Located RdrName
               -- name of the type
               -> ConDecl GhcPs
               -- constructor declaration
               -> GhcTags

    mkConsTags tyName ConDeclGADT { con_names, con_args } =
         (\n -> mkGhcTagForMember n tyName TkGADTConstructor)
         `map` con_names
      ++ mkHsConDeclDetails tyName con_args

    mkConsTags tyName ConDeclH98  { con_name, con_args } =
        mkGhcTagForMember con_name tyName TkDataConstructor
      : mkHsConDeclDetails tyName con_args

    mkConsTags _ XConDecl {} = []

    mkHsConDeclDetails :: Located RdrName -> HsConDeclDetails GhcPs -> GhcTags
    mkHsConDeclDetails tyName (RecCon (L _ fields)) =
        foldl' f [] fields
      where
        f :: GhcTags -> LConDeclField GhcPs -> GhcTags
        f ts (L _ ConDeclField { cd_fld_names }) = foldl' g ts cd_fld_names
        f ts _ = ts

        g :: GhcTags -> LFieldOcc GhcPs -> GhcTags
        g ts (L _ FieldOcc { rdrNameFieldOcc }) =
            mkGhcTagForMember rdrNameFieldOcc tyName TkRecordField
          : ts
        g ts _ = ts

    mkHsConDeclDetails _ _ = []


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

        -- abstraction binding is only used after translation
        AbsBinds {} -> []

        PatSynBind _ PSB { psb_id } -> [mkGhcTag' psb_id TkPatternSynonym]
        PatSynBind _ XPatSynBind {} -> []

        XHsBindsLR {} -> []


    mkClsMemberTags :: Located RdrName -> Sig GhcPs -> GhcTags
    mkClsMemberTags clsName (TypeSig   _ lhs _) =
      (\n -> mkGhcTagForMember n clsName TkTypeSignature)
      `map` lhs
    mkClsMemberTags clsName (PatSynSig _ lhs _) =
      (\n -> mkGhcTagForMember n clsName TkPatternSynonym)
      `map` lhs
    mkClsMemberTags clsName (ClassOpSig _ _ lhs _) =
      (\n ->  mkGhcTagForMember n clsName TkTypeClassMember)
      `map` lhs
    mkClsMemberTags _ _ = []


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
                     -> Maybe (Located RdrName)
                     -- if this type family is associate, pass the name of the
                     -- associated class
                     -> Maybe GhcTag
    mkFamilyDeclTags FamilyDecl { fdLName, fdInfo } assocClsName =
      case assocClsName of
        Nothing      -> Just $ mkGhcTag' fdLName tk
        Just clsName -> Just $ mkGhcTagForMember fdLName clsName tk 
      where
        tk = case fdInfo of
              DataFamily           -> TkDataTypeFamily
              OpenTypeFamily       -> TkTypeFamily
              ClosedTypeFamily {}  -> TkTypeFamily
    mkFamilyDeclTags XFamilyDecl {} _ = Nothing


    -- used to generate tag of an instance declaration
    mkLHsTypeTag :: LHsType GhcPs -> Maybe GhcTag
    mkLHsTypeTag (L _ hsType) = (\a -> mkGhcTag a TkTypeClassInstance []) <$> hsTypeTagName hsType


    hsTypeTagName :: HsType GhcPs -> Maybe (Located RdrName)
    hsTypeTagName hsType =
      case hsType of
        HsForAllTy {hst_body} -> hsTypeTagName (unLoc hst_body)
        
        HsQualTy {hst_body}   -> hsTypeTagName (unLoc hst_body)

        HsTyVar _ _ a         -> Just $ a

        HsAppTy _ a _         -> hsTypeTagName (unLoc a)
        HsOpTy _ _ a _        -> Just $ a
        HsKindSig _ a _       -> hsTypeTagName (unLoc a)

        _                     -> Nothing


    -- data family instance declaration
    --
    mkDataFamInstDeclTag :: DataFamInstDecl GhcPs -> GhcTags
    mkDataFamInstDeclTag DataFamInstDecl { dfid_eqn } =
      case dfid_eqn of
        XHsImplicitBndrs {} -> []

        HsIB { hsib_body = FamEqn { feqn_tycon, feqn_rhs } } ->
          case feqn_rhs of
            HsDataDefn { dd_cons } ->
                mkGhcTag' feqn_tycon TkDataTypeFamilyInstance
              : (mkConsTags feqn_tycon . unLoc) `concatMap` dd_cons
            XHsDataDefn {} ->
              mkGhcTag' feqn_tycon TkDataTypeFamilyInstance : []

        HsIB { hsib_body = XFamEqn {} } -> []


    -- type family instance declaration
    --
    mkTyFamInstDeclTag :: TyFamInstDecl GhcPs -> Maybe GhcTag
    mkTyFamInstDeclTag TyFamInstDecl { tfid_eqn } =
      case tfid_eqn of
        XHsImplicitBndrs {} -> Nothing

        -- TODO: should we check @feqn_rhs :: LHsType GhcPs@ as well?
        HsIB { hsib_body = FamEqn { feqn_tycon } } ->
          Just $ mkGhcTag' feqn_tycon TkTypeFamilyInstance

        HsIB { hsib_body = XFamEqn {} } -> Nothing
