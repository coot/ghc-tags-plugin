{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generate tags from @'HsModule' 'GhcPs'@ representation.
--
module GhcTags.Ghc
  ( GhcTag (..)
  , GhcTags
  , GhcTagKind (..)
  , getGhcTags
  ) where


import           Data.Maybe    (mapMaybe)
import           Data.Foldable (foldl')
import           Data.ByteString (ByteString)

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
                              , FamilyResultSig (..)
                              , ForeignDecl (..)
                              , LHsDecl
                              , HsConDeclDetails
                              , HsDecl (..)
                              , HsDataDefn (..)
                              , InstDecl (..)
                              , TyClDecl (..)
                              , TyFamInstDecl (..)
                              , hsConDeclArgTys
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
                              , HsKind
                              , HsType (..)
                              , HsWildCardBndrs
                              , LConDeclField
                              , LHsType
                              , LHsSigType
                              , HsTyVarBndr (..)
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


-- | Kind of the term.
--
data GhcTagKind
    = GtkTerm
    | GtkFunction
    | GtkTypeConstructor        (Maybe (HsKind GhcPs))

    -- | H98 data construtor
    | GtkDataConstructor        (Located RdrName) -- ^ type name
                                [HsType GhcPs]    -- ^ fields type

    -- | GADT constructor with its type
    | GtkGADTConstructor               (HsType GhcPs)
    | GtkRecordField
    | GtkTypeSynonym                   (HsType GhcPs)
    | GtkTypeSignature                 (HsWildCardBndrs GhcPs (LHsSigType GhcPs))
    | GtkPatternSynonym
    | GtkTypeClass
    | GtkTypeClassMember
    | GtkTypeClassInstance             (HsType GhcPs)
    | GtkTypeFamily             (Maybe (HsKind GhcPs))
    | GtkTypeFamilyInstance
    | GtkDataTypeFamily         (Maybe (HsKind GhcPs))
    | GtkDataTypeFamilyInstance
    | GtkForeignImport
    | GtkForeignExport


-- | We can read names from using fields of type 'GHC.Hs.Extensions.IdP' (a type
-- family) which for @'Parsed@ resolved to 'RdrName'
--
data GhcTag = GhcTag {
    gtSrcSpan    :: !SrcSpan
    -- ^ term location
  , gtTag        :: !ByteString
    -- ^ utf8 encoded tag's name
  , gtKind       :: !GhcTagKind
    -- ^ tag's kind
  , gtIsExported :: !Bool
    -- ^ 'True' iff the term is exported
  , gtFFI        :: !(Maybe String)
    -- ^ @ffi@ import
  }

type GhcTags = [GhcTag]


-- | Check if an identifier is exported.
--
isExported :: Maybe [IE GhcPs] -> Located RdrName -> Bool
isExported Nothing   _name = True
isExported (Just ies) (L _ name) =
    any (\ie -> ieName ie == Just name) ies
  where
    -- TODO: the GHC's one is partial, and I got a panic error.
    ieName :: IE GhcPs -> Maybe RdrName
    ieName (IEVar _ (L _ n))              = Just $ ieWrappedName n
    ieName (IEThingAbs  _ (L _ n))        = Just $ ieWrappedName n
    ieName (IEThingWith _ (L _ n) _ _ _)  = Just $ ieWrappedName n
    ieName (IEThingAll  _ (L _ n))        = Just $ ieWrappedName n
    ieName _ = Nothing


-- | Check if a class member or a type constructors is exported.
--
isMemberExported :: Maybe [IE GhcPs]
                 -> Located RdrName -- member name / constructor name
                 -> Located RdrName -- type class name / type constructor name
                 -> Bool
isMemberExported Nothing    _memberName _className = True
isMemberExported (Just ies) memberName  className  = any go ies
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
         -> GhcTagKind
         -- ^ tag's kind
         -> Bool
         -- ^ is term exported
         -> GhcTag
mkGhcTag (L gtSrcSpan rdrName) gtKind gtIsExported =
    case rdrName of
      Unqual occName ->
        GhcTag { gtTag = fs_bs (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      Qual _ occName ->
        GhcTag { gtTag = fs_bs (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      -- Orig is the only one we are interested in
      Orig _ occName ->
        GhcTag { gtTag = fs_bs (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      Exact eName -> 
        GhcTag { gtTag = fs_bs (occNameFS (nameOccName eName))
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }


-- | Generate tags for a module - simple walk over the syntax tree.
--
-- Supported identifiers:
--
--  * /top level terms/
--  * /data types/
--  * /record fields/
--  * /type synonyms/
--  * /type classes/
--  * /type class members/
--  * /type class instances/
--  * /type families/
--  * /type family instances/
--  * /data type families/
--  * /data type families instances/
--  * /data type family instances constructors/
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
              -> GhcTagKind
              -- ^ tag's kind
              -> GhcTag
    mkGhcTag' a k = mkGhcTag a k (isExported mies a)

    mkGhcTagForMember :: Located RdrName -- member name
                      -> Located RdrName -- class name
                      -> GhcTagKind
                      -> GhcTag
    mkGhcTagForMember memberName className kind =
      mkGhcTag memberName kind
        (isMemberExported mies memberName className)


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
          SynDecl { tcdLName, tcdRhs = L _ hsType } ->
            mkGhcTag' tcdLName (GtkTypeSynonym hsType) : tags

          -- data declaration:
          --   type,
          --   constructors,
          --   record fields
          --
          DataDecl { tcdLName, tcdDataDefn } -> 
            case tcdDataDefn of
              HsDataDefn { dd_cons, dd_kindSig } ->
                     mkGhcTag' tcdLName (GtkTypeConstructor (unLoc <$> dd_kindSig))
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
              mkGhcTag' tcdLName GtkTypeClass
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
                        Just a  -> mkGhcTag' a GtkTypeFamilyInstance : tags'
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
                  SourceText s -> tag { gtFFI = Just s }
              : tags
            where
              tag = mkGhcTag' fd_name GtkForeignImport

          ForeignExport { fd_name } ->
              mkGhcTag' fd_name GtkForeignExport
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

    mkConsTags tyName ConDeclGADT { con_names, con_args, con_res_ty = L _ con_res_ty } =
         (\n -> mkGhcTagForMember n tyName (GtkGADTConstructor con_res_ty))
         `map` con_names
      ++ mkHsConDeclDetails tyName con_args

    mkConsTags tyName ConDeclH98  { con_name, con_args } =
        mkGhcTagForMember con_name tyName
          (GtkDataConstructor tyName (map unLoc $ hsConDeclArgTys con_args))
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
            mkGhcTagForMember rdrNameFieldOcc tyName GtkRecordField
          : ts
        g ts _ = ts

    mkHsConDeclDetails _ _ = []


    mkHsBindLRTags :: HsBindLR GhcPs GhcPs -> GhcTags
    mkHsBindLRTags hsBind =
      case hsBind of
        FunBind { fun_id } -> [mkGhcTag' fun_id GtkFunction]

        -- TODO
        -- This is useful fo generating tags for
        -- ````
        -- Just x = lhs
        -- ```
        PatBind {} -> []

        VarBind { var_id, var_rhs = L srcSpan _ } -> [mkGhcTag' (L srcSpan var_id) GtkTerm]

        -- abstraction binding is only used after translation
        AbsBinds {} -> []

        PatSynBind _ PSB { psb_id } -> [mkGhcTag' psb_id GtkPatternSynonym]
        PatSynBind _ XPatSynBind {} -> []

        XHsBindsLR {} -> []


    mkClsMemberTags :: Located RdrName -> Sig GhcPs -> GhcTags
    mkClsMemberTags clsName (TypeSig   _ lhs hsSigWcType) =
      (\n -> mkGhcTagForMember n clsName (GtkTypeSignature hsSigWcType))
      `map` lhs
    mkClsMemberTags clsName (PatSynSig _ lhs _) =
      (\n -> mkGhcTagForMember n clsName GtkPatternSynonym)
      `map` lhs
    mkClsMemberTags clsName (ClassOpSig _ _ lhs _) =
      (\n ->  mkGhcTagForMember n clsName GtkTypeClassMember)
      `map` lhs
    mkClsMemberTags _ _ = []


    mkSigTags :: Sig GhcPs -> GhcTags
    mkSigTags (TypeSig   _ lhs hsSigWcType)
                                     = flip mkGhcTag' (GtkTypeSignature hsSigWcType)
                                         `map` lhs
    mkSigTags (PatSynSig _ lhs _)    = flip mkGhcTag' GtkPatternSynonym  `map` lhs
    mkSigTags (ClassOpSig _ _ lhs _) = flip mkGhcTag' GtkTypeClassMember `map` lhs
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
    mkFamilyDeclTags FamilyDecl { fdLName, fdInfo, fdResultSig = L _ familyResultSig } assocClsName =
      case assocClsName of
        Nothing      -> Just $ mkGhcTag' fdLName tk
        Just clsName -> Just $ mkGhcTagForMember fdLName clsName tk 
      where
        tk = case fdInfo of
              DataFamily           -> GtkDataTypeFamily (famResultKindSignature familyResultSig)
              OpenTypeFamily       -> GtkTypeFamily (famResultKindSignature familyResultSig)
              ClosedTypeFamily {}  -> GtkTypeFamily (famResultKindSignature familyResultSig)
    mkFamilyDeclTags XFamilyDecl {} _ = Nothing


    -- used to generate tag of an instance declaration
    mkLHsTypeTag :: LHsType GhcPs -> Maybe GhcTag
    mkLHsTypeTag (L _ hsType) = (\a -> mkGhcTag a (GtkTypeClassInstance hsType) True) <$> hsTypeTagName hsType


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
                mkGhcTag' feqn_tycon GtkDataTypeFamilyInstance
              : (mkConsTags feqn_tycon . unLoc) `concatMap` dd_cons
            XHsDataDefn {} ->
              mkGhcTag' feqn_tycon GtkDataTypeFamilyInstance : []

        HsIB { hsib_body = XFamEqn {} } -> []


    -- type family instance declaration
    --
    mkTyFamInstDeclTag :: TyFamInstDecl GhcPs -> Maybe GhcTag
    mkTyFamInstDeclTag TyFamInstDecl { tfid_eqn } =
      case tfid_eqn of
        XHsImplicitBndrs {} -> Nothing

        -- TODO: should we check @feqn_rhs :: LHsType GhcPs@ as well?
        HsIB { hsib_body = FamEqn { feqn_tycon } } ->
          Just $ mkGhcTag' feqn_tycon GtkTypeFamilyInstance

        HsIB { hsib_body = XFamEqn {} } -> Nothing

--
--
--

famResultKindSignature :: FamilyResultSig GhcPs -> Maybe (HsKind GhcPs)
famResultKindSignature (NoSig _) = Nothing
famResultKindSignature (KindSig _ ki) = Just (unLoc ki)
famResultKindSignature (TyVarSig _ bndr) =
  case unLoc bndr of
    UserTyVar _ _ -> Nothing
    KindedTyVar _ _ ki -> Just (unLoc ki)
    XTyVarBndr {} -> Nothing
famResultKindSignature XFamilyResultSig {} = Nothing
