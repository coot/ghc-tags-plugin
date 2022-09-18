{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 810
#define GHC_IMPORT(NAME) GHC.Hs.NAME
#else
#define GHC_IMPORT(NAME) Hs ## NAME
#endif


-- | Generate tags from @'HsModule' 'GhcPs'@ representation.
--
module GhcTags.Ghc
  ( GhcTag (..)
  , GhcTags
  , GhcTagKind (..)
  , getGhcTags
  , hsDeclsToGhcTags
  ) where


import           Data.Maybe    (mapMaybe)
import           Data.Foldable (foldl')
import           Data.ByteString (ByteString)

-- Ghc imports
#if   __GLASGOW_HASKELL__ >= 902
import           GHC.Types.SourceText (SourceText (..))
#elif __GLASGOW_HASKELL__ >= 900
import           GHC.Types.Basic (SourceText (..))
#else
import           BasicTypes      (SourceText (..))
#endif
#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Data.FastString (bytesFS)
#elif __GLASGOW_HASKELL__ >= 810
import           FastString          (bytesFS)
#else
import           FastString          (FastString (fs_bs))
#endif
#if   __GLASGOW_HASKELL__ >= 900 && __GLASGOW_HASKELL__ < 902
import           GHC.Types.FieldLabel (FieldLbl (..))
#elif __GLASGOW_HASKELL__ < 900
import           FieldLabel           (FieldLbl (..))
#endif
import           GHC_IMPORT(Binds)
                              ( HsBindLR (..)
                              , PatSynBind (..)
                              , Sig (..)
                              )
import           GHC_IMPORT(Decls)
                              ( ForeignImport (..)
                              , ClsInstDecl (..)
                              , ConDecl (..)
                              , DataFamInstDecl (..)
                              , FamEqn (..)
                              , FamilyDecl (..)
                              , FamilyInfo (..)
                              , FamilyResultSig (..)
                              , ForeignDecl (..)
                              , LHsDecl
#if   __GLASGOW_HASKELL__ >= 902
                              , HsConDeclH98Details
#else
                              , HsConDeclDetails
#endif
                              , HsDecl (..)
                              , HsDataDefn (..)
                              , InstDecl (..)
                              , TyClDecl (..)
                              , TyFamInstDecl (..)
                              )
#if   __GLASGOW_HASKELL__ >= 810
import           GHC.Hs.Decls ( StandaloneKindSig (..) )
#endif
import           GHC_IMPORT(ImpExp)
                              ( IE (..)
                              , IEWildcard (..)
                              , ieWrappedName
                              )
import           GHC_IMPORT(Extension)
                              ( GhcPs
                              )

#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Hs.Type
#elif __GLASGOW_HASKELL__ >= 810
import           GHC.Hs.Types
#else
import           HsTypes
#endif
                              ( ConDeclField (..)
                              , FieldOcc (..)
                              , HsConDetails (..)
#if   __GLASGOW_HASKELL__ < 902
                              , HsImplicitBndrs (..)
#endif
                              , HsKind
                              , HsTyVarBndr (..)
                              , HsType (..)
                              , HsWildCardBndrs
                              , LConDeclField
                              , LFieldOcc
                              , LHsQTyVars (..)
                              , LHsSigType
                              , LHsType
                              )

#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Types.SrcLoc
                                ( GenLocated (..)
                                , Located
                                , SrcSpan (..)
                                , unLoc
                                )
import           GHC.Types.Name.Reader
                                ( RdrName (..)
                                , rdrNameOcc
                                )
import           GHC.Types.Name ( nameOccName
                                , occNameFS
                                )
#else
import           SrcLoc         ( GenLocated (..)
                                , Located
                                , SrcSpan (..)
                                , unLoc
                                )
import           RdrName        ( RdrName (..)
                                , rdrNameOcc
                                )
import           Name           ( nameOccName
                                , occNameFS
                                )
#endif
#if   __GLASGOW_HASKELL__ >= 902
import           GHC.Hs       ( HsModule (..)
                              , HsSigType (..)
                              , HsConDeclGADTDetails (..)
                              )
import           GHC.Parser.Annotation (SrcSpanAnn' (..))
#elif __GLASGOW_HASKELL__ >= 810
import           GHC.Hs       ( HsModule (..) )
#else
import           HsSyn        ( HsModule (..) )
#endif

#if __GLASGOW_HASKELL__ < 902
type HsConDeclH98Details ps = HsConDeclDetails ps
#endif

#if __GLASGOW_HASKELL__ >= 900
type GhcPsModule = HsModule
type GhcPsHsTyVarBndr = HsTyVarBndr () GhcPs
#else
type GhcPsModule = HsModule GhcPs
type GhcPsHsTyVarBndr = HsTyVarBndr    GhcPs
#endif

#if __GLASGOW_HASKELL__ < 810
bytesFS :: FastString -> ByteString
bytesFS = fs_bs
#endif


-- | Kind of the term.
--
data GhcTagKind
    = GtkTerm
    | GtkFunction
    | GtkTypeConstructor        (Maybe (HsKind GhcPs))

    -- | H98 data constructor
    | GtkDataConstructor               (ConDecl GhcPs)

    -- | GADT constructor with its type
    | GtkGADTConstructor               (ConDecl GhcPs)
    | GtkRecordField
    | GtkTypeSynonym                   (HsType GhcPs)
    | GtkTypeSignature                 (HsWildCardBndrs GhcPs (LHsSigType GhcPs))
    | GtkTypeKindSignature             (LHsSigType GhcPs)
    | GtkPatternSynonym
    | GtkTypeClass
    | GtkTypeClassMember               (HsType GhcPs)
    | GtkTypeClassInstance             (HsType GhcPs)
    | GtkTypeClassInstanceMember       (HsType GhcPs)
    | GtkTypeFamily             (Maybe ([GhcPsHsTyVarBndr], Either (HsKind GhcPs) GhcPsHsTyVarBndr))
    -- ghc-8.6.5 does not provide 'TyFamInstDecl' for associated type families
    | GtkTypeFamilyInstance     (Maybe (TyFamInstDecl GhcPs))
    | GtkDataTypeFamily         (Maybe ([GhcPsHsTyVarBndr], Either (HsKind GhcPs) GhcPsHsTyVarBndr))
    | GtkDataTypeFamilyInstance (Maybe (HsKind GhcPs))
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
#if __GLASGOW_HASKELL__ < 902
    ieName (IEThingWith _ (L _ n) _ _ _)  = Just $ ieWrappedName n
#else
    ieName (IEThingWith _ (L _ n) _ _)    = Just $ ieWrappedName n
#endif
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

#if __GLASGOW_HASKELL__ < 902
    go (IEThingWith _ _ IEWildcard{} _ _) = True
#else
    go (IEThingWith _ _ IEWildcard{} _)   = True
#endif

#if __GLASGOW_HASKELL__ < 902
    go (IEThingWith _ (L _ n) NoIEWildcard ns lfls) =
#else
    go (IEThingWith _ (L _ n) NoIEWildcard ns) =
#endif
            ieWrappedName n == unLoc className
#if __GLASGOW_HASKELL__ < 902
         && (isInWrappedNames || isInFieldLbls)
#else
         &&  isInWrappedNames
#endif
      where
        -- the 'NameSpace' does not agree between things that are in the 'IE'
        -- list and passed member or type class names (constructor / type
        -- constructor names, respectively)
        isInWrappedNames = any ((== occNameFS (rdrNameOcc (unLoc memberName))) . occNameFS . rdrNameOcc . ieWrappedName . unLoc) ns
#if __GLASGOW_HASKELL__ < 902
        isInFieldLbls    = any ((== occNameFS (rdrNameOcc (unLoc memberName))) . occNameFS . rdrNameOcc . flSelector. unLoc) lfls
#endif

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
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      Qual _ occName ->
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      -- Orig is the only one we are interested in
      Orig _ occName ->
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      Exact eName ->
        GhcTag { gtTag = bytesFS (occNameFS (nameOccName eName))
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
getGhcTags :: Located GhcPsModule
           -> GhcTags
getGhcTags (L _ HsModule { hsmodDecls, hsmodExports }) =
    hsDeclsToGhcTags mies hsmodDecls
  where
    mies :: Maybe [IE GhcPs]
    mies = map unLoc . unLoc <$> hsmodExports


hsDeclsToGhcTags :: Maybe [IE GhcPs]
                 -> [LHsDecl GhcPs]
                 -> GhcTags
hsDeclsToGhcTags mies =
    reverse . foldl' go []
  where
    fixLoc :: SrcSpan -> GhcTag -> GhcTag
    fixLoc loc gt@GhcTag { gtSrcSpan = UnhelpfulSpan {} } = gt { gtSrcSpan = loc }
    fixLoc _   gt                                      = gt

    -- like 'mkGhcTag' but checks if the identifier is exported
    mkGhcTag' :: SrcSpan
              -- ^ declaration's location; it is useful when the term does not
              -- contain useful information (e.g. code generated from template
              -- haskell splices).
              ->  Located RdrName
              --  ^ @RdrName ~ IdP GhcPs@ it *must* be a name of a top level
              --  identifier.
              -> GhcTagKind
              -- ^ tag's kind
              -> GhcTag
    mkGhcTag' l a k = fixLoc l $ mkGhcTag a k (isExported mies a)


    mkGhcTagForMember :: SrcSpan
                      -- ^ declaration's 'SrcSpan'
                      -> Located RdrName -- member name
                      -> Located RdrName -- class name
                      -> GhcTagKind
                      -> GhcTag
    mkGhcTagForMember decLoc memberName className kind =
      fixLoc decLoc $ mkGhcTag memberName kind
                               (isMemberExported mies memberName className)

    -- Main routine which traverse all top level declarations.
    --
    go :: GhcTags -> LHsDecl GhcPs -> GhcTags
    go tags (L decLoc' hsDecl) = let decLoc = locAnn decLoc' in case hsDecl of

      -- type or class declaration
      TyClD _ tyClDecl ->
        case tyClDecl of

          -- type family declarations
          FamDecl { tcdFam } ->
            case mkFamilyDeclTags decLoc tcdFam Nothing of
              Just tag -> tag : tags
              Nothing  ->       tags

          -- type synonyms
          SynDecl { tcdLName, tcdRhs = L _ hsType } ->
            mkGhcTag' decLoc (unSpanAnn tcdLName) (GtkTypeSynonym hsType) : tags

          -- data declaration:
          --   type,
          --   constructors,
          --   record fields
          --
          DataDecl { tcdLName, tcdDataDefn } ->
            case tcdDataDefn of
              HsDataDefn { dd_cons, dd_kindSig } ->
                     mkGhcTag' decLoc (unSpanAnn tcdLName) (GtkTypeConstructor (unLoc <$> dd_kindSig))
                   : (mkConsTags decLoc (unSpanAnn tcdLName) . unLoc) `concatMap` dd_cons
                  ++ tags

#if __GLASGOW_HASKELL__ < 900
              XHsDataDefn {} -> tags
#endif

          -- Type class declaration:
          --   type class name,
          --   type class members,
          --   default methods,
          --   default data type instance
          --
          ClassDecl { tcdLName, tcdSigs, tcdMeths, tcdATs, tcdATDefs } ->
               -- class name
               mkGhcTag' decLoc (unSpanAnn tcdLName) GtkTypeClass
               -- class methods
             : (mkClsMemberTags decLoc (unSpanAnn tcdLName) . unLoc) `concatMap` tcdSigs
               -- default methods
            ++ foldl' (\tags' hsBind -> mkHsBindLRTags decLoc (unLoc hsBind) ++ tags')
                     []
                     tcdMeths
            -- associated types
            ++ ((\a -> mkFamilyDeclTags decLoc a (Just $ unSpanAnn tcdLName)) . unLoc) `mapMaybe` tcdATs
            -- associated type defaults (data type families, type families
            -- (open or closed)
            ++ foldl'
#if __GLASGOW_HASKELL__ < 810
                (\tags' (L _ tyFamDeflEqn) ->
                  let decl = Nothing in
#elif __GLASGOW_HASKELL__ < 902
                (\tags' (L _ decl'@(TyFamInstDecl HsIB { hsib_body = tyFamDeflEqn })) ->
                  let decl = Just decl' in
#else
                (\tags' (L _ decl'@(TyFamInstDecl { tfid_eqn = tyFamDeflEqn })) ->
                  let decl = Just decl' in
#endif
                    case tyFamDeflEqn of
                      FamEqn { feqn_rhs = L _ hsType } ->
                        case hsTypeTagName hsType of
                          -- TODO: add a `default` field
                          Just a  -> mkGhcTag' decLoc a (GtkTypeFamilyInstance decl) : tags'
                          Nothing -> tags'
#if __GLASGOW_HASKELL__ < 900
                      XFamEqn {} -> tags'
#endif
                )
                [] tcdATDefs
            ++ tags

#if __GLASGOW_HASKELL__ < 900
          XTyClDecl {} -> tags
#endif

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
#if __GLASGOW_HASKELL__ < 900
              XClsInstDecl {} -> tags
#endif

              ClsInstDecl { cid_poly_ty, cid_tyfam_insts, cid_datafam_insts, cid_binds } ->
                  case cid_poly_ty of
#if __GLASGOW_HASKELL__ < 900
                    XHsImplicitBndrs {} ->
                      tyFamTags ++ dataFamTags ++ tags
#endif

                    -- TODO: @hsbib_body :: LHsType GhcPs@
#if __GLASGOW_HASKELL__ < 902
                    HsIB { hsib_body = body } ->
#else
                    L _ HsSig { sig_body = body } ->
#endif
                      case mkLHsTypeTag decLoc body of
                        Nothing  ->       map (fixTagKind (unLoc body)) (tyFamTags ++ dataFamTags ++ bindsTags) ++ tags
                        Just tag -> tag : map (fixTagKind (unLoc body)) (tyFamTags ++ dataFamTags ++ bindsTags) ++ tags
                where
                  -- associated type and data type family instances
                  dataFamTags = (mkDataFamInstDeclTag decLoc . unLoc) `concatMap` cid_datafam_insts
                  tyFamTags   = (mkTyFamInstDeclTag   decLoc . unLoc) `mapMaybe`  cid_tyfam_insts
                  bindsTags   = foldl' (\tags' hsBind -> mkHsBindLRTags decLoc (unLoc hsBind) ++ tags') [] cid_binds

                  fixTagKind body a = a { gtKind = GtkTypeClassInstanceMember body }

          -- data family instance
          DataFamInstD { dfid_inst } ->
            mkDataFamInstDeclTag decLoc dfid_inst ++ tags

          -- type family instance
          TyFamInstD { tfid_inst } ->
            case mkTyFamInstDeclTag decLoc tfid_inst of
              Nothing  ->       tags
              Just tag -> tag : tags

#if __GLASGOW_HASKELL__ < 900
          XInstDecl {} -> tags
#endif

      -- deriving declaration
      DerivD {} -> tags

      -- value declaration
      ValD _ hsBind  -> mkHsBindLRTags decLoc hsBind ++ tags

      -- signature declaration
      SigD _ sig -> mkSigTags decLoc sig ++ tags

#if __GLASGOW_HASKELL__ >= 810
      -- standalone kind signatures
      KindSigD _ stdKindSig ->
        case stdKindSig of
          StandaloneKindSig _ ksName sigType ->
           mkGhcTag' decLoc (unSpanAnn ksName)  (GtkTypeKindSignature sigType) : tags

#if __GLASGOW_HASKELL__ < 900
          XStandaloneKindSig {} -> tags
#endif
#endif

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
              tag = mkGhcTag' decLoc (unSpanAnn fd_name) GtkForeignImport

          ForeignExport { fd_name } ->
              mkGhcTag' decLoc (unSpanAnn fd_name) GtkForeignExport
            : tags

#if __GLASGOW_HASKELL__ < 900
          XForeignDecl {} -> tags
#endif

      WarningD {}   -> tags
      AnnD {}       -> tags

      -- TODO: Rules are named it would be nice to get them too
      RuleD {}      -> tags
      SpliceD {}    -> tags
      DocD {}       -> tags
      RoleAnnotD {} -> tags
#if __GLASGOW_HASKELL__ < 900
      XHsDecl {}    -> tags
#endif


    -- generate tags of all constructors of a type
    --
    mkConsTags :: SrcSpan
               -> Located RdrName
               -- name of the type
               -> ConDecl GhcPs
               -- constructor declaration
               -> GhcTags

#if __GLASGOW_HASKELL__ < 902
    mkConsTags decLoc tyName con@ConDeclGADT { con_names, con_args } =
#else
    mkConsTags decLoc tyName con@ConDeclGADT { con_names, con_g_args = con_args } =
#endif
         ( (\n -> mkGhcTagForMember decLoc n tyName (GtkGADTConstructor con))
         . unSpanAnn )
         `map` con_names
      ++ mkHsConDeclGADTDetails decLoc tyName con_args

    mkConsTags decLoc tyName con@ConDeclH98  { con_name, con_args } =
        mkGhcTagForMember decLoc (unSpanAnn con_name) tyName
          (GtkDataConstructor con)
      : mkHsConDeclH98Details decLoc tyName con_args

#if __GLASGOW_HASKELL__ < 900
    mkConsTags _ _ XConDecl {} = []
#endif

    mkHsConDeclH98Details :: SrcSpan
                          -> Located RdrName
                          -> HsConDeclH98Details GhcPs
                          -> GhcTags
    mkHsConDeclH98Details decLoc tyName (RecCon (L _ fields)) =
        foldl' f [] fields
      where
        f :: GhcTags -> LConDeclField GhcPs -> GhcTags
        f ts (L _ ConDeclField { cd_fld_names }) = foldl' g ts cd_fld_names
#if __GLASGOW_HASKELL__ < 900
        f ts _ = ts
#endif

        g :: GhcTags -> LFieldOcc GhcPs -> GhcTags
        g ts (L _ fo@FieldOcc {}) =
#if __GLASGOW_HASKELL__ >= 904
            mkGhcTagForMember decLoc (unSpanAnn $ foLabel fo) tyName GtkRecordField
#else
            mkGhcTagForMember decLoc (unSpanAnn $ rdrNameFieldOcc fo) tyName GtkRecordField
#endif
          : ts
#if __GLASGOW_HASKELL__ < 900
        g ts _ = ts
#endif

    mkHsConDeclH98Details _ _ _ = []

    mkHsConDeclGADTDetails :: SrcSpan
                           -> Located RdrName
#if __GLASGOW_HASKELL__ < 902
                           -> HsConDeclH98Details  GhcPs
#else
                           -> HsConDeclGADTDetails GhcPs
#endif
                           -> GhcTags
#if __GLASGOW_HASKELL__ < 902
    mkHsConDeclGADTDetails = mkHsConDeclH98Details
#else
#if __GLASGOW_HASKELL__ < 904
    mkHsConDeclGADTDetails decLoc tyName (RecConGADT (L _ fields)) =
#else
    mkHsConDeclGADTDetails decLoc tyName (RecConGADT (L _ fields) _) =
#endif
        foldl' f [] fields
      where
        f :: GhcTags -> LConDeclField GhcPs -> GhcTags
        f ts (L _ ConDeclField { cd_fld_names }) = foldl' g ts cd_fld_names

        g :: GhcTags -> LFieldOcc GhcPs -> GhcTags
        g ts (L _ fo) =
#if __GLASGOW_HASKELL__ >= 904
            mkGhcTagForMember decLoc (unSpanAnn $ foLabel fo) tyName GtkRecordField
#else
            mkGhcTagForMember decLoc (unSpanAnn $ rdrNameFieldOcc fo) tyName GtkRecordField
#endif
          : ts
    mkHsConDeclGADTDetails _ _ _ = []
#endif


    mkHsBindLRTags :: SrcSpan
                   -- ^ declaration's 'SrcSpan'
                   -> HsBindLR GhcPs GhcPs
                   -> GhcTags
    mkHsBindLRTags decLoc hsBind =
      case hsBind of
        FunBind { fun_id } -> [mkGhcTag' decLoc (unSpanAnn fun_id) GtkFunction]

        -- TODO
        -- This is useful fo generating tags for
        -- ````
        -- Just x = lhs
        -- ```
        PatBind {} -> []

        VarBind { var_id, var_rhs = L srcSpan _ } ->
          [mkGhcTag' decLoc (unSpanAnn $ L srcSpan var_id) GtkTerm]

#if __GLASGOW_HASKELL__ < 904
        -- abstraction binding is only used after translation
        AbsBinds {} -> []
#endif

        PatSynBind _ PSB { psb_id } -> [mkGhcTag' decLoc (unSpanAnn psb_id) GtkPatternSynonym]
#if __GLASGOW_HASKELL__ < 900
        PatSynBind _ XPatSynBind {} -> []
#endif

#if __GLASGOW_HASKELL__ < 900
        XHsBindsLR {} -> []
#endif


    mkClsMemberTags :: SrcSpan -> Located RdrName -> Sig GhcPs -> GhcTags
    mkClsMemberTags decLoc clsName (TypeSig   _ lhs hsSigWcType) =
      ( (\n -> mkGhcTagForMember decLoc n clsName (GtkTypeSignature hsSigWcType))
      . unSpanAnn )
      `map` lhs
    mkClsMemberTags decLoc clsName (PatSynSig _ lhs _) =
      ( (\n -> mkGhcTagForMember decLoc n clsName GtkPatternSynonym)
      . unSpanAnn )
      `map` lhs
#if __GLASGOW_HASKELL__ < 902
    mkClsMemberTags decLoc clsName (ClassOpSig _ _ lhs HsIB { hsib_body = L _ hsType}) =
#else
    mkClsMemberTags decLoc clsName (ClassOpSig _ _ lhs (L _ hsType)) =
#endif
      ( (\n -> mkGhcTagForMember decLoc n clsName
                                (GtkTypeClassMember $ hsSigTypeToHsType hsType))
      . unSpanAnn )
     `map` lhs
    mkClsMemberTags _ _ _ = []


    mkSigTags :: SrcSpan -> Sig GhcPs -> GhcTags
    mkSigTags decLoc (TypeSig   _ lhs hsSigWcType)
                                       = ( flip (mkGhcTag' decLoc)
                                                (GtkTypeSignature hsSigWcType)
                                         . unSpanAnn )
                                         `map` lhs
    mkSigTags decLoc (PatSynSig _ lhs _)
                                       = ( flip (mkGhcTag' decLoc) GtkPatternSynonym
                                         . unSpanAnn )
                                         `map` lhs
#if __GLASGOW_HASKELL__ < 902
    mkSigTags decLoc (ClassOpSig _ _ lhs HsIB { hsib_body = L _ hsType })
#else
    mkSigTags decLoc (ClassOpSig _ _ lhs (L _ hsType))
#endif
                                       = ( flip (mkGhcTag' decLoc)
                                                ( GtkTypeClassMember
                                                $ hsSigTypeToHsType hsType )
                                         . unSpanAnn
                                         )
                                         `map` lhs
#if __GLASGOW_HASKELL__ < 900
    mkSigTags _ (ClassOpSig _ _ _ XHsImplicitBndrs {})
                                       = []
#endif
    mkSigTags _ IdSig {}               = []
    -- TODO: generate theses with additional info (fixity)
    mkSigTags _ FixSig {}              = []
    mkSigTags _ InlineSig {}           = []
    -- SPECIALISE pragmas
    mkSigTags _ SpecSig {}             = []
    mkSigTags _ SpecInstSig {}         = []
    -- MINIMAL pragma
    mkSigTags _ MinimalSig {}          = []
    -- SSC pragma
    mkSigTags _ SCCFunSig {}           = []
    -- COMPLETE pragma
    mkSigTags _ CompleteMatchSig {}    = []
#if __GLASGOW_HASKELL__ < 900
    mkSigTags _ XSig {}                = []
#endif


    mkFamilyDeclTags :: SrcSpan
                     -> FamilyDecl GhcPs
                     -- ^ declaration's 'SrcSpan'
                     -> Maybe (Located RdrName)
                     -- if this type family is associate, pass the name of the
                     -- associated class
                     -> Maybe GhcTag
    mkFamilyDeclTags decLoc FamilyDecl { fdLName, fdInfo, fdTyVars, fdResultSig = L _ familyResultSig } assocClsName =
      case assocClsName of
        Nothing      -> Just $ mkGhcTag' decLoc (unSpanAnn fdLName) tk
        Just clsName -> Just $ mkGhcTagForMember decLoc (unSpanAnn fdLName) clsName tk
      where

        mb_fdvars = case fdTyVars of
          HsQTvs { hsq_explicit } -> Just $ unLoc `map` hsq_explicit
#if __GLASGOW_HASKELL__ < 900
          XLHsQTyVars {} -> Nothing
#endif
        mb_resultsig = famResultKindSignature familyResultSig

        mb_typesig = (,) <$> mb_fdvars <*> mb_resultsig

        tk = case fdInfo of
              DataFamily           -> GtkDataTypeFamily mb_typesig
              OpenTypeFamily       -> GtkTypeFamily     mb_typesig
              ClosedTypeFamily {}  -> GtkTypeFamily     mb_typesig
#if __GLASGOW_HASKELL__ < 900
    mkFamilyDeclTags _ XFamilyDecl {} _ = Nothing
#endif


    -- used to generate tag of an instance declaration
    mkLHsTypeTag :: SrcSpan
                 -- declartaion's 'SrcSpan'
                 -> LHsType GhcPs
                 -> Maybe GhcTag
    mkLHsTypeTag decLoc (L _ hsType) =
      (\a -> fixLoc decLoc $ mkGhcTag a (GtkTypeClassInstance hsType) True)
      <$> hsTypeTagName hsType


    hsTypeTagName :: HsType GhcPs -> Maybe (Located RdrName)
    hsTypeTagName hsType =
      case hsType of
        HsForAllTy {hst_body} -> hsTypeTagName (unLoc hst_body)

        HsQualTy {hst_body}   -> hsTypeTagName (unLoc hst_body)

        HsTyVar _ _ a         -> Just $ unSpanAnn a

        HsAppTy _ a _         -> hsTypeTagName (unLoc a)
#if __GLASGOW_HASKELL__ >= 904
        HsOpTy _ _ _ a _        -> Just $ unSpanAnn a
#else
        HsOpTy _ _ a _        -> Just $ unSpanAnn a
#endif
        HsKindSig _ a _       -> hsTypeTagName (unLoc a)

        _                     -> Nothing


    -- data family instance declaration
    --
    mkDataFamInstDeclTag :: SrcSpan -> DataFamInstDecl GhcPs -> GhcTags
    mkDataFamInstDeclTag decLoc DataFamInstDecl { dfid_eqn } =
      case dfid_eqn of
#if __GLASGOW_HASKELL__ < 900
        XHsImplicitBndrs {} -> []
#endif

#if __GLASGOW_HASKELL__ < 902
        HsIB { hsib_body = FamEqn { feqn_tycon, feqn_rhs } } ->
#else
        FamEqn { feqn_tycon, feqn_rhs } ->
#endif
          case feqn_rhs of
            HsDataDefn { dd_cons, dd_kindSig } ->
                mkGhcTag' decLoc (unSpanAnn feqn_tycon)
                          (GtkDataTypeFamilyInstance
                            (unLoc <$> dd_kindSig))
              : (mkConsTags decLoc (unSpanAnn feqn_tycon) . unLoc)
                `concatMap` dd_cons

#if __GLASGOW_HASKELL__ < 900
            XHsDataDefn {} ->
              [mkGhcTag' decLoc feqn_tycon (GtkDataTypeFamilyInstance Nothing)]

        HsIB { hsib_body = XFamEqn {} } -> []
#endif


    -- type family instance declaration
    --
    mkTyFamInstDeclTag :: SrcSpan -> TyFamInstDecl GhcPs -> Maybe GhcTag
    mkTyFamInstDeclTag decLoc decl@TyFamInstDecl { tfid_eqn } =
      case tfid_eqn of
#if __GLASGOW_HASKELL__ < 900
        XHsImplicitBndrs {} -> Nothing
#endif

        -- TODO: should we check @feqn_rhs :: LHsType GhcPs@ as well?
#if __GLASGOW_HASKELL__ < 902
        HsIB { hsib_body = FamEqn { feqn_tycon } } ->
#else
        FamEqn { feqn_tycon } ->
#endif
          Just $ mkGhcTag' decLoc (unSpanAnn feqn_tycon) (GtkTypeFamilyInstance (Just decl))

#if __GLASGOW_HASKELL__ < 900
        HsIB { hsib_body = XFamEqn {} } -> Nothing
#endif

#if __GLASGOW_HASKELL__ < 902
unSpanAnn :: Located RdrName -> Located RdrName
unSpanAnn = id
#else
unSpanAnn :: GenLocated (SrcSpanAnn' x) RdrName -> Located RdrName
unSpanAnn (L s a) = L (locA s) a
#endif

#if __GLASGOW_HASKELL__ < 902
locAnn :: SrcSpan -> SrcSpan
locAnn = id
#else
locAnn :: SrcSpanAnn' a -> SrcSpan
locAnn = locA
#endif

#if __GLASGOW_HASKELL__ < 902
hsSigTypeToHsType :: HsType GhcPs -> HsType GhcPs
hsSigTypeToHsType = id
#else
hsSigTypeToHsType :: HsSigType GhcPs -> HsType GhcPs
hsSigTypeToHsType = unLoc . sig_body
#endif

--
--
--

famResultKindSignature :: FamilyResultSig GhcPs
                       -> Maybe (Either (HsKind GhcPs) GhcPsHsTyVarBndr)
famResultKindSignature (NoSig _)           = Nothing
famResultKindSignature (KindSig _ ki)      = Just (Left (unLoc ki))
famResultKindSignature (TyVarSig _ bndr)   = Just (Right (unLoc bndr))
#if __GLASGOW_HASKELL__ < 900
famResultKindSignature XFamilyResultSig {} = Nothing
#endif
