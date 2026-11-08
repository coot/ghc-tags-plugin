{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_GHC(8,10)
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
  , Filter (..)
  , PatternMatches (..)
  , getGhcTags
  , hsDeclsToGhcTags
  ) where


import           Data.Containers.ListUtils (nubOrdOn)
import           Data.List     (partition)
import           Data.Maybe    (mapMaybe)
import           Data.Maybe    (maybeToList)
import qualified Data.Foldable as Foldable
import           Data.ByteString (ByteString)
import           Data.Set (Set)
import qualified Data.Set as Set

-- Ghc imports
import           GHC.Types.SourceText (SourceText (..))
#if   MIN_VERSION_GHC(9,8)
import qualified GHC.Data.FastString as FastString
#endif
import           GHC.Data.FastString (bytesFS)
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
                              , HsConDeclH98Details
                              , HsDecl (..)
                              , HsDataDefn (..)
                              , InstDecl (..)
                              , TyClDecl (..)
                              , TyFamInstDecl (..)
                              )
import           GHC.Hs.Decls ( StandaloneKindSig (..) )
import           GHC_IMPORT(ImpExp)
                              ( IE (..)
                              , IEWildcard (..)
                              , ieWrappedName
                              )
import           GHC_IMPORT(Extension)
                              ( GhcPs
                              )

import           GHC.Hs.Type
#if MIN_VERSION_GHC(9,14)
                              ( HsConDeclRecField (..)
                              , LHsConDeclRecField
#else
                              ( ConDeclField (..)
                              , LConDeclField
#endif
                              , FieldOcc (..)
                              , HsConDetails (..)
                              , HsKind
                              , HsTyVarBndr (..)
                              , HsType (..)
                              , HsWildCardBndrs
                              , mkHsWildCardBndrs
                              , LFieldOcc
                              , LHsQTyVars (..)
                              , LHsSigType
                              , LHsType
                              )

import           GHC.Types.SrcLoc
                                ( GenLocated (..)
                                , Located
                                , SrcSpan (..)
                                , srcSpanFile
                                , srcSpanStartLine
                                , unLoc
                                )
import           GHC.Types.Name.Reader
                                ( RdrName (..)
                                , mkRdrUnqual
                                , rdrNameOcc
                                )
import           GHC.Types.Name ( isBuiltInSyntax
                                , nameOccName
                                , occNameFS
                                )
import           GHC.Hs       ( HsConDeclGADTDetails (..)
                              , HsModule (..)
                              , HsSigType (..)
#if   MIN_VERSION_GHC(9,6)
                              , CImportSpec (..) 
#endif
#if   MIN_VERSION_GHC(9,10)
                              , HasLoc
                              , SrcSpanAnnA
                              , locA
#endif
                              )
#if   MIN_VERSION_GHC(9,6)
import           GHC.Types.ForeignCall (CCallTarget (..))
#endif
#if  !MIN_VERSION_GHC(9,10)
import           GHC.Parser.Annotation (SrcSpanAnn' (..))
#endif
import           GHC.Hs       ( GRHSs (..)
                              , Pat (..)
                              , LPat
                              , HsRecFields (..)
                              , HsFieldBind (..)
                              , hsConPatArgs
                              , HsLocalBinds
                              , HsLocalBindsLR (..)
                              , HsValBindsLR (..)
                              , Match (..)
                              , MatchGroup (..)
                              )
import           GHC.Hs       ( AnnDecl (..)
                              , ArithSeqInfo (..)
                              , GRHS (..)
                              , HsCmd (..)
                              , HsCmdTop (..)
                              , HsExpr (..)
                              , HsIPBinds (..)
                              , HsPatSynDir (..)
                              , HsQuote (..)
                              , HsTupArg (..)
#if MIN_VERSION_GHC(9,14)
                              , HsTypedSplice (..)
#endif
                              , HsUntypedSplice (..)
                              , IPBind (..)
                              , LGRHS
                              , LHsCmd
                              , LHsCmdTop
                              , LHsExpr
#if MIN_VERSION_GHC(9,8)
                              , LHsRecUpdFields (..)
#endif
                              , LMatch
                              , LStmtLR
                              , ParStmtBlock (..)
                              , RuleDecl (..)
                              , RuleDecls (..)
                              , SpliceDecl (..)
                              , StmtLR (..)
                              )
#if MIN_VERSION_GHC(9,6)
import           Language.Haskell.Syntax.Module.Name (moduleNameFS)
#else
import           GHC.Unit.Module.Name (moduleNameFS)
#endif

#if MIN_VERSION_GHC(9,6)
type GhcPsModule = HsModule GhcPs
type GhcPsHsTyVarBndr = HsTyVarBndr () GhcPs
#else
type GhcPsModule = HsModule
type GhcPsHsTyVarBndr = HsTyVarBndr () GhcPs
#endif


-- | Kind of the term.
--
data GhcTagKind
    = GtkModule
    | GtkTerm
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

    -- | constructor or pattern synonym matched by a pattern
    | GtkPatternMatch


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
    -- ^ 'True' iff the term is exported; always 'True' for pattern matches,
    -- which are not file-local
  , gtIsLocal    :: !Bool
    -- ^ 'True' iff the binding is nested inside another declaration
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
#if MIN_VERSION_GHC(9,10)
    ieName (IEVar _ (L _ n) _)            = Just $ ieWrappedName n
    ieName (IEThingAbs  _ (L _ n) _)      = Just $ ieWrappedName n
    ieName (IEThingWith _ (L _ n) _ _ _)  = Just $ ieWrappedName n
    ieName (IEThingAll  _ (L _ n) _)      = Just $ ieWrappedName n
    ieName _ = Nothing
#else
    ieName (IEVar _ (L _ n))              = Just $ ieWrappedName n
    ieName (IEThingAbs  _ (L _ n))        = Just $ ieWrappedName n
    ieName (IEThingWith _ (L _ n) _ _)    = Just $ ieWrappedName n
    ieName (IEThingAll  _ (L _ n))        = Just $ ieWrappedName n
    ieName _ = Nothing
#endif


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

#if MIN_VERSION_GHC(9,10)
    go (IEVar _ (L _ n) _) = ieWrappedName n == unLoc memberName

    go (IEThingAbs _ (L _ n) _) = ieWrappedName n == unLoc memberName

    go (IEThingAll _ (L _ n) _) = ieWrappedName n `elem` [unLoc memberName, unLoc className]

    go (IEThingWith _ (L _ n) IEWildcard{} _ _) = ieWrappedName n `elem` [unLoc memberName, unLoc className]

    go (IEThingWith _ (L _ n) NoIEWildcard ns _) =
            ieWrappedName n == unLoc memberName
         || (ieWrappedName n == unLoc className && isInWrappedNames)
#else
    go (IEVar _ (L _ n)) = ieWrappedName n == unLoc memberName

    go (IEThingAbs _ (L _ n)) = ieWrappedName n == unLoc memberName

    go (IEThingAll _ (L _ n)) = ieWrappedName n `elem` [unLoc memberName, unLoc className]

    go (IEThingWith _ (L _ n) IEWildcard{} _) = ieWrappedName n `elem` [unLoc memberName, unLoc className]

    go (IEThingWith _ (L _ n) NoIEWildcard ns) =
            ieWrappedName n == unLoc memberName
         || (ieWrappedName n == unLoc className && isInWrappedNames)
#endif
      where
        -- the 'NameSpace' does not agree between things that are in the 'IE'
        -- list and passed member or type class names (constructor / type
        -- constructor names, respectively)
        isInWrappedNames =
            any ((== occNameFS (rdrNameOcc (unLoc memberName))))
          . map (occNameFS . rdrNameOcc . ieWrappedName . unLoc)
          $ ns

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
               , gtIsLocal = False
               , gtFFI = Nothing
               }

      Qual _ occName ->
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtIsLocal = False
               , gtFFI = Nothing
               }

      -- Orig is the only one we are interested in
      Orig _ occName ->
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtIsLocal = False
               , gtFFI = Nothing
               }

      Exact eName ->
        GhcTag { gtTag = bytesFS (occNameFS (nameOccName eName))
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtIsLocal = False
               , gtFFI = Nothing
               }


-- | How to filter tags.
--
data Filter =
    RemoveTerms
    -- ^ remove close terms, preserve type signatures
  | RetainAll
    -- ^ preserve all types
  deriving (Eq, Show)

-- | Whether to tag constructors and pattern synonyms matched by patterns, e.g.
-- in function arguments, @case@ alternatives, lambdas, @do@ binds, guards or
-- pattern bindings.  Built-in syntax, e.g. @[]@, @()@, tuples or @(:)@, is not
-- tagged.  Types and pragmas, other than @RULES@ and @ANN@, are not searched,
-- e.g. Template Haskell splices in types.
--
-- Pattern matches are not declarations, so their tags are neither local nor
-- file-local: editors, e.g. vim, prefer file-local tags in the current file
-- to declarations in other files.  Tags files cannot distinguish columns,
-- hence there is at most one tag for each name and line.
--
data PatternMatches =
    ExcludePatternMatches
    -- ^ tag only declarations
  | IncludePatternMatches
    -- ^ also tag pattern matches, using 'GtkPatternMatch'
  deriving (Eq, Show)

-- | Generate tags for a module - simple walk over the syntax tree.
--
-- Supported identifiers:
--
--  * /module name/
--  * /top level terms/
--  * /local bindings/
--  * /data types/
--  * /record fields/
--  * /type synonyms/
--  * /type classes/
--  * /type class members/
--  * /type class instances/
--  * /type class instance members/
--  * /type families/
--  * /type family instances/
--  * /data type families/
--  * /data type families instances/
--  * /data type family instances constructors/
--  * /constructors and pattern synonyms matched by patterns/, see
--    'PatternMatches'
--
getGhcTags :: Filter
           -- ^ whether to filter out term/function tags that have a type
           -- signature in the same binding group
           -> PatternMatches
           -- ^ whether to tag pattern matches
           -> Located GhcPsModule
           -> GhcTags
getGhcTags filterBindings patternMatches (L _ HsModule { hsmodName, hsmodDecls, hsmodExports }) =
       maybeToList (mkModNameTag <$> hsmodName)
    ++
       hsDeclsToGhcTags filterBindings patternMatches mies hsmodDecls
  where
    mies :: Maybe [IE GhcPs]
    mies = map unLoc . unLoc <$> hsmodExports

    mkModNameTag (L loc modName) =
      GhcTag { gtSrcSpan    = locA loc
             , gtTag        = bytesFS $ moduleNameFS modName
             , gtKind       = GtkModule
             , gtIsExported = True
             , gtIsLocal    = False
             , gtFFI        = Nothing
             }


hsDeclsToGhcTags :: Filter
                 -> PatternMatches
                 -> Maybe [IE GhcPs]
                 -> [LHsDecl GhcPs]
                 -> GhcTags
hsDeclsToGhcTags filterBindings patternMatches mies decls =
    onePatternMatchPerLine (reverse (Foldable.foldl' go [] decls))
  where
    signatures = signatureNames [ sig | L _ (SigD _ sig) <- decls ]

    -- Only signatures in the current binding group can replace a definition.
    signatureNames :: [Sig GhcPs] -> Set RdrName
    signatureNames sigs = Set.fromList
      [ unLoc name
      | sig <- sigs
      , name <- case sig of
          TypeSig    _ names _   -> names
          PatSynSig  _ names _   -> names
          ClassOpSig _ _ names _ -> names
          _                      -> []
      ]

    fixLoc :: SrcSpan -> GhcTag -> GhcTag
    fixLoc loc gt@GhcTag { gtSrcSpan = UnhelpfulSpan {} } = gt { gtSrcSpan = loc }
    fixLoc _   gt                                         = gt

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

    -- pattern-match tags, if they are enabled; pattern matches are not
    -- declarations, so they are neither local nor file-local
    mkPatternMatchTags :: SrcSpan
                       -- ^ declaration's 'SrcSpan'
                       -> [Located RdrName]
                       -- ^ matched constructors and pattern synonyms
                       -> GhcTags
    mkPatternMatchTags decLoc names =
      case patternMatches of
        ExcludePatternMatches -> []
        IncludePatternMatches ->
          [ fixLoc decLoc $ mkGhcTag name GtkPatternMatch True | name <- names ]

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

          -- Type class declaration:
          --   type class name,
          --   type class members,
          --   default methods,
          --   default data type instance
          --
          ClassDecl { tcdLName, tcdSigs, tcdMeths, tcdATs, tcdATDefs } ->
            let signedMethods = signatureNames (map unLoc tcdSigs)
            in
               -- class name
               mkGhcTag' decLoc (unSpanAnn tcdLName) GtkTypeClass
               -- class methods
             : (mkClsMemberTags decLoc (unSpanAnn tcdLName) . unLoc) `concatMap` tcdSigs
               -- default methods
            ++ Foldable.foldl' (\tags' hsBind ->
                                    uncurry (++)
                                      (mkHsBindLRTags
                                        (\name -> isMemberExported mies name (unSpanAnn tcdLName))
                                        signedMethods decLoc (unLoc hsBind))
                                 ++ tags')
                               []
                               tcdMeths
            -- associated types
            ++ ((\a -> mkFamilyDeclTags decLoc a (Just $ unSpanAnn tcdLName)) . unLoc) `mapMaybe` tcdATs
            -- associated type defaults
            ++ (mkTyFamInstDeclTag
                  (\name -> isMemberExported mies name (unSpanAnn tcdLName)) decLoc . unLoc)
               `mapMaybe` tcdATDefs
            ++ tags

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

              ClsInstDecl { cid_poly_ty, cid_tyfam_insts, cid_datafam_insts, cid_binds, cid_sigs } ->
                  case cid_poly_ty of

                    -- TODO: @hsbib_body :: LHsType GhcPs@
                    L _ HsSig { sig_body = body } ->
                      case mkLHsTypeTag decLoc body of
                        Nothing  ->       map (fixTagKind (unLoc body)) (tyFamTags ++ dataFamTags ++ bindsTags ++ sigsTags) ++ bindsPatternMatchTags ++ tags
                        Just tag -> tag : map (fixTagKind (unLoc body)) (tyFamTags ++ dataFamTags ++ bindsTags ++ sigsTags) ++ bindsPatternMatchTags ++ tags
                where
                  -- Instances are exported regardless of an export list, and
                  -- so are their members; constructors and record fields of
                  -- data instances follow the export list.
                  --
                  -- associated type and data type family instances
                  dataFamTags = (mkDataFamInstDeclTag decLoc . unLoc) `concatMap` cid_datafam_insts
                  tyFamTags   = (mkTyFamInstDeclTag (const True) decLoc . unLoc) `mapMaybe`  cid_tyfam_insts
                  className = hsTypeTagName (hsSigTypeToHsType $ unLoc cid_poly_ty)
                  -- pattern matches in methods are not instance members
                  (bindsTags, bindsPatternMatchTags) =
                                (mkHsBindLRTags (const True) (signatureNames (map unLoc cid_sigs)) decLoc . unLoc)
                                `foldMap` cid_binds
                  sigsTags    = (map (\tag -> tag { gtIsExported = True })
                                . maybe (mkSigTags decLoc) (mkClsMemberTags decLoc) className . unLoc)
                                `concatMap` cid_sigs

                  fixTagKind body a = a { gtKind = GtkTypeClassInstanceMember body }

          -- data family instance; exported like class instances
          DataFamInstD { dfid_inst } ->
            mkDataFamInstDeclTag decLoc dfid_inst ++ tags

          -- type family instance; exported like class instances
          TyFamInstD { tfid_inst } ->
            case mkTyFamInstDeclTag (const True) decLoc tfid_inst of
              Nothing  ->       tags
              Just tag -> tag : tags


      -- deriving declaration
      DerivD {} -> tags

      -- value declaration
      ValD _ hsBind  -> uncurry (++) (mkHsBindLRTags (isExported mies) signatures decLoc hsBind) ++ tags

      -- signature declaration
      SigD _ sig -> mkSigTags decLoc sig ++ tags

      -- standalone kind signatures
      KindSigD _ stdKindSig ->
        case stdKindSig of
          StandaloneKindSig _ ksName sigType ->
           mkGhcTag' decLoc (unSpanAnn ksName)  (GtkTypeKindSignature sigType) : tags

      -- default declaration
      DefD {} -> tags

      -- foreign declaration
      ForD _ foreignDecl ->
        case foreignDecl of
#if MIN_VERSION_GHC(9,6)
          ForeignImport { fd_fi = CImport _ _ _mheader _ CLabel {} } -> tags
          ForeignImport { fd_fi = CImport _ _ _mheader _ CWrapper } -> tags
          ForeignImport { fd_fi = CImport _ _ _mheader _ (CFunction DynamicTarget) } -> tags
          ForeignImport { fd_fi = CImport _ _ _mheader _ (CFunction ((StaticTarget sourceText _ _ _))), fd_name } ->
#else
          ForeignImport { fd_name, fd_fi = CImport _ _ _mheader _ (L _ sourceText) } ->
#endif
                case sourceText of
                  NoSourceText -> tag
                  -- TODO: add header information from '_mheader'
#if MIN_VERSION_GHC(9,8)
                  SourceText s -> tag { gtFFI = Just (FastString.unpackFS s) }
#else
                  SourceText s -> tag { gtFFI = Just s }
#endif
              : tags
            where
              tag = mkGhcTag' decLoc (unSpanAnn fd_name) GtkForeignImport

          ForeignExport { fd_name } ->
              mkGhcTag' decLoc (unSpanAnn fd_name) GtkForeignExport
            : tags

      WarningD {}   -> tags
      AnnD {}       -> mkPatternMatchTags decLoc (declConPats hsDecl) ++ tags

      -- TODO: Rules are named it would be nice to get them too
      RuleD {}      -> mkPatternMatchTags decLoc (declConPats hsDecl) ++ tags
      SpliceD {}    -> mkPatternMatchTags decLoc (declConPats hsDecl) ++ tags
      DocD {}       -> tags
      RoleAnnotD {} -> tags

    -- generate tags of all constructors of a type
    --
    mkConsTags :: SrcSpan
               -> Located RdrName
               -- name of the type
               -> ConDecl GhcPs
               -- constructor declaration
               -> GhcTags

    mkConsTags decLoc tyName con@ConDeclGADT { con_names, con_g_args = con_args } =
         ( (\n -> mkGhcTagForMember decLoc n tyName (GtkGADTConstructor con))
         . unSpanAnn )
         `map` con_names'
      ++ mkHsConDeclGADTDetails decLoc tyName con_args
      where
        con_names' = Foldable.toList con_names

    mkConsTags decLoc tyName con@ConDeclH98  { con_name, con_args } =
        mkGhcTagForMember decLoc (unSpanAnn con_name) tyName
          (GtkDataConstructor con)
      : mkHsConDeclH98Details decLoc tyName con_args

    -- tags of declarations, and of pattern matches, in local bindings
    mkHsLocalBindsTags :: SrcSpan -> HsLocalBinds GhcPs -> (GhcTags, GhcTags)
    mkHsLocalBindsTags decLoc (HsValBinds _ (ValBinds _ hsBindsLR sigs)) =
        ( -- where clause bindings
          map markLocal
            (bindsTags
          ++ map (\tag -> tag {gtIsExported = False}) (concatMap (mkSigTags decLoc . unLoc) sigs))
        , bindsPatternMatchTags
        )
      where
        (bindsTags, bindsPatternMatchTags) =
          (mkHsBindLRTags (const False) (signatureNames (map unLoc sigs)) decLoc . unLoc)
          `foldMap` hsBindsLR
        markLocal tag = tag {gtIsLocal = True}

    -- implicit parameters are not tagged
    mkHsLocalBindsTags decLoc binds = ([], mkPatternMatchTags decLoc (localBindsConPats binds))

    mkHsConDeclH98Details :: SrcSpan
                          -> Located RdrName
                          -> HsConDeclH98Details GhcPs
                          -> GhcTags
    mkHsConDeclH98Details decLoc tyName (RecCon (L _ fields)) =
        Foldable.foldl' f [] fields
      where
#if MIN_VERSION_GHC(9,14)
        f :: GhcTags -> LHsConDeclRecField GhcPs -> GhcTags
        f ts (L _ HsConDeclRecField { cdrf_names }) = Foldable.foldl' g ts cdrf_names
#else
        f :: GhcTags -> LConDeclField GhcPs -> GhcTags
        f ts (L _ ConDeclField { cd_fld_names }) = Foldable.foldl' g ts cd_fld_names
#endif

        g :: GhcTags -> LFieldOcc GhcPs -> GhcTags
        g ts (L _ fo@FieldOcc {}) =
            mkGhcTagForMember decLoc (unSpanAnn $ foLabel fo) tyName GtkRecordField
          : ts

    mkHsConDeclH98Details _ _ _ = []

    mkHsConDeclGADTDetails :: SrcSpan
                           -> Located RdrName
                           -> HsConDeclGADTDetails GhcPs
                           -> GhcTags
#if MIN_VERSION_GHC(9,14)
    mkHsConDeclGADTDetails decLoc tyName (RecConGADT _ (L _ fields)) =
#elif MIN_VERSION_GHC(9,10)
    mkHsConDeclGADTDetails decLoc tyName (RecConGADT _ (L _ fields)) =
#else
    mkHsConDeclGADTDetails decLoc tyName (RecConGADT (L _ fields) _) =
#endif
        Foldable.foldl' f [] fields
      where
#if MIN_VERSION_GHC(9,14)
        f :: GhcTags -> LHsConDeclRecField GhcPs -> GhcTags
        f ts (L _ HsConDeclRecField { cdrf_names }) = Foldable.foldl' g ts cdrf_names
#else
        f :: GhcTags -> LConDeclField GhcPs -> GhcTags
        f ts (L _ ConDeclField { cd_fld_names }) = Foldable.foldl' g ts cd_fld_names
#endif

        g :: GhcTags -> LFieldOcc GhcPs -> GhcTags
        g ts (L _ fo) =
            mkGhcTagForMember decLoc (unSpanAnn $ foLabel fo) tyName GtkRecordField
          : ts
    mkHsConDeclGADTDetails _ _ _ = []


    -- tags of declarations, and of pattern matches, in a binding
    mkHsBindLRTags :: (Located RdrName -> Bool)
                   -- ^ visibility of bindings directly in this scope
                   -> Set RdrName
                   -- ^ names with signatures in this binding group
                   -> SrcSpan
                   -- ^ declaration's 'SrcSpan'
                   -> HsBindLR GhcPs GhcPs
                   -> (GhcTags, GhcTags)
    mkHsBindLRTags exported signedNames decLoc hsBind =
      case hsBind of
        FunBind { fun_id, fun_matches = MG { mg_alts = L _ matches } } ->
             (bindingTag (unSpanAnn fun_id) GtkFunction, [])
          <> foldMap (mkMatchTags . unLoc) matches

        PatBind { pat_lhs, pat_rhs } ->
             ( concatMap (\name -> bindingTag name GtkTerm) (patternBinders pat_lhs)
             , mkPatternMatchTags decLoc (patConPats pat_lhs)
             )
          <> mkGRHSsTags pat_rhs

        VarBind { var_id, var_rhs = L srcSpan _ } ->
          ( bindingTag (unSpanAnn $ L srcSpan var_id) GtkTerm
          , mkPatternMatchTags decLoc (bindConPats hsBind)
          )

        PatSynBind _ PSB { psb_id } ->
          ( bindingTag (unSpanAnn psb_id) GtkPatternSynonym
          , mkPatternMatchTags decLoc (bindConPats hsBind)
          )
      where
        bindingTag name kind
          | RemoveTerms <- filterBindings
          , unLoc name `Set.member` signedNames
          = []
          | otherwise = [fixLoc decLoc $ mkGhcTag name kind (exported name)]

        -- local bindings are declarations, the rest of a match contains only
        -- pattern matches
        mkMatchTags :: Match GhcPs (LHsExpr GhcPs) -> (GhcTags, GhcTags)
        mkMatchTags match =
             ([], mkPatternMatchTags decLoc (concatMap patConPats (matchPats match)))
          <> mkGRHSsTags (m_grhss match)

        mkGRHSsTags :: GRHSs GhcPs (LHsExpr GhcPs) -> (GhcTags, GhcTags)
        mkGRHSsTags GRHSs { grhssGRHSs, grhssLocalBinds } =
             ([], mkPatternMatchTags decLoc (concatMap (grhsConPats exprConPats) grhssGRHSs))
          <> mkHsLocalBindsTags decLoc grhssLocalBinds


    -- Preserve binder locations and ignore expressions, constructors and type
    -- variables occurring in patterns. Record puns have a placeholder RHS in
    -- the parsed AST; their variable name and location come from the label.
    -- Implicit record-wildcard fields are only available after renaming.
    patternBinders :: LPat GhcPs -> [Located RdrName]
    patternBinders (L _ pat) = case pat of
      VarPat _ name -> [unSpanAnn name]
#if MIN_VERSION_GHC(9,10)
      AsPat _ name nested -> unSpanAnn name : patternBinders nested
      ParPat _ nested -> patternBinders nested
#else
      AsPat _ name _ nested -> unSpanAnn name : patternBinders nested
      ParPat _ _ nested _ -> patternBinders nested
#endif
      LazyPat _ nested -> patternBinders nested
      BangPat _ nested -> patternBinders nested
      ListPat _ pats -> concatMap patternBinders pats
      TuplePat _ pats _ -> concatMap patternBinders pats
      SumPat _ nested _ _ -> patternBinders nested
      ConPat {pat_args = RecCon HsRecFields {rec_flds}} ->
        concatMap (\(L _ HsFieldBind {hfbLHS, hfbRHS, hfbPun}) ->
          if hfbPun
            then [fmap (mkRdrUnqual . rdrNameOcc) (unSpanAnn $ foLabel $ unLoc hfbLHS)]
            else patternBinders hfbRHS) rec_flds
      ConPat {pat_args} -> concatMap patternBinders (hsConPatArgs pat_args)
      ViewPat _ _ nested -> patternBinders nested
      SigPat _ nested _ -> patternBinders nested
      NPlusKPat _ name _ _ _ _ -> [unSpanAnn name]
      _ -> []


    mkClsMemberTags :: SrcSpan -> Located RdrName -> Sig GhcPs -> GhcTags
    mkClsMemberTags decLoc clsName (TypeSig   _ lhs hsSigWcType) =
      ( (\n -> mkGhcTagForMember decLoc n clsName (GtkTypeSignature hsSigWcType))
      . unSpanAnn )
      `map` lhs
    mkClsMemberTags decLoc clsName (PatSynSig _ lhs hsSigType) =
      ( (\n -> mkGhcTagForMember decLoc n clsName
                                (GtkTypeSignature $ mkHsWildCardBndrs hsSigType))
      . unSpanAnn )
      `map` lhs
    mkClsMemberTags decLoc clsName (ClassOpSig _ _ lhs (L _ hsType)) =
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
    mkSigTags decLoc (PatSynSig _ lhs hsSigType)
                                       = ( flip (mkGhcTag' decLoc)
                                                (GtkTypeSignature $ mkHsWildCardBndrs hsSigType)
                                         . unSpanAnn )
                                         `map` lhs
    mkSigTags decLoc (ClassOpSig _ _ lhs (L _ hsType))
                                       = ( flip (mkGhcTag' decLoc)
                                                ( GtkTypeClassMember
                                                $ hsSigTypeToHsType hsType )
                                         . unSpanAnn
                                         )
                                         `map` lhs
#if !MIN_VERSION_GHC(9,6)
    mkSigTags _ IdSig {}               = []
#endif
    -- TODO: generate theses with additional info (fixity)
    mkSigTags _ FixSig {}              = []
    mkSigTags _ InlineSig {}           = []
    -- SPECIALISE pragmas
    mkSigTags _ SpecSig {}             = []
    mkSigTags _ SpecInstSig {}         = []
#if MIN_VERSION_GHC(9,14) && !MIN_VERSION_GHC(9,18)
    -- DEPRECATED SPECIALISE pragma
    mkSigTags _ (SpecSigE _ _ _ _) = []
#endif
    -- MINIMAL pragma
    mkSigTags _ MinimalSig {}          = []
    -- SSC pragma
    mkSigTags _ SCCFunSig {}           = []
    -- COMPLETE pragma
    mkSigTags _ CompleteMatchSig {}    = []


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

        mb_fdvars :: Maybe [GhcPsHsTyVarBndr]
        mb_fdvars = case fdTyVars of
#if MIN_VERSION_GHC(9,8)
          HsQTvs {} -> Nothing
#else
          HsQTvs { hsq_explicit } -> Just $ unLoc `map` hsq_explicit
#endif
        mb_resultsig :: Maybe (Either (HsKind GhcPs) GhcPsHsTyVarBndr)
        mb_resultsig = famResultKindSignature familyResultSig

        mb_typesig :: Maybe ([GhcPsHsTyVarBndr], Either (HsKind GhcPs) GhcPsHsTyVarBndr)
        mb_typesig = (,) <$> mb_fdvars <*> mb_resultsig

        tk = case fdInfo of
              DataFamily           -> GtkDataTypeFamily mb_typesig
              OpenTypeFamily       -> GtkTypeFamily     mb_typesig
              ClosedTypeFamily {}  -> GtkTypeFamily     mb_typesig


    -- used to generate tag of an instance declaration
    mkLHsTypeTag :: SrcSpan
                 -- declaration's 'SrcSpan'
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
        HsOpTy _ _ _ a _      -> Just $ unSpanAnn a
        HsKindSig _ a _       -> hsTypeTagName (unLoc a)

        _                     -> Nothing


    -- data family instance declaration; the instance is exported, its
    -- constructors and record fields follow the export list
    --
    mkDataFamInstDeclTag :: SrcSpan -> DataFamInstDecl GhcPs -> GhcTags
    mkDataFamInstDeclTag decLoc DataFamInstDecl { dfid_eqn } =
      case dfid_eqn of

        FamEqn { feqn_tycon, feqn_rhs } ->
          case feqn_rhs of
            HsDataDefn { dd_cons, dd_kindSig } ->
                (fixLoc decLoc $
                  mkGhcTag (unSpanAnn feqn_tycon)
                           (GtkDataTypeFamilyInstance (unLoc <$> dd_kindSig))
                           True)
              : (mkConsTags decLoc (unSpanAnn feqn_tycon) . unLoc)
                `concatMap` dd_cons


    -- type family instance declaration
    --
    mkTyFamInstDeclTag :: (Located RdrName -> Bool) -> SrcSpan -> TyFamInstDecl GhcPs -> Maybe GhcTag
    mkTyFamInstDeclTag exported decLoc decl@TyFamInstDecl { tfid_eqn } =
      case tfid_eqn of

        -- TODO: should we check @feqn_rhs :: LHsType GhcPs@ as well?
        FamEqn { feqn_tycon } ->
          Just $ fixLoc decLoc $
            mkGhcTag (unSpanAnn feqn_tycon)
                     (GtkTypeFamilyInstance (Just decl))
                     (exported $ unSpanAnn feqn_tycon)


-- | Keep the first pattern-match tag for each name and line: tags files cannot
-- distinguish columns.
--
onePatternMatchPerLine :: GhcTags -> GhcTags
onePatternMatchPerLine tags =
    declarations ++ nubOrdOn tagLine patternMatches
  where
    (patternMatches, declarations) = partition isPatternMatch tags

    isPatternMatch GhcTag { gtKind = GtkPatternMatch } = True
    isPatternMatch _                                   = False

    tagLine GhcTag { gtTag, gtSrcSpan } =
      ( gtTag
      , case gtSrcSpan of
          RealSrcSpan span_ _ -> Just (bytesFS (srcSpanFile span_), srcSpanStartLine span_)
          UnhelpfulSpan {}    -> Nothing
      )


--
-- Constructors and pattern synonyms matched by patterns, in source order.
-- 'hsDeclsToGhcTags' tags declarations, including local bindings in @where@
-- clauses, and uses these functions for everything else, so that each node is
-- visited once; only the left-hand sides of pattern bindings are also searched
-- for bound variables.  Types cannot contain patterns, except in Template
-- Haskell splices, so they are not traversed.
--

patConPats :: LPat GhcPs -> [Located RdrName]
patConPats (L _ pat) =
    case pat of
      WildPat {}             -> []
      VarPat {}              -> []
      LazyPat _ p            -> patConPats p
#if MIN_VERSION_GHC(9,10)
      AsPat _ _ p            -> patConPats p
      ParPat _ p             -> patConPats p
#else
      AsPat _ _ _ p          -> patConPats p
      ParPat _ _ p _         -> patConPats p
#endif
      BangPat _ p            -> patConPats p
      ListPat _ ps           -> concatMap patConPats ps
      TuplePat _ ps _        -> concatMap patConPats ps
      SumPat _ p _ _         -> patConPats p
      ConPat { pat_con, pat_args }
        | builtInSyntax (unLoc pat_con)
                             -> args
        | otherwise          -> unSpanAnn pat_con : args
        where
          args = concatMap patConPats (hsConPatArgs pat_args)
      ViewPat _ e p          -> exprConPats e ++ patConPats p
      SplicePat _ splice     -> untypedSpliceConPats splice
      LitPat {}              -> []
      NPat {}                -> []
      NPlusKPat {}           -> []
      SigPat _ p _           -> patConPats p
#if MIN_VERSION_GHC(9,10)
      EmbTyPat {}            -> []
      InvisPat {}            -> []
#endif
#if MIN_VERSION_GHC(9,12)
      OrPat _ ps             -> concatMap patConPats ps
#endif
  where
    -- e.g. @[]@, @()@, tuples or @(:)@
    builtInSyntax (Exact name) = isBuiltInSyntax name
    builtInSyntax _            = False


exprConPats :: LHsExpr GhcPs -> [Located RdrName]
exprConPats (L _ expr) =
    case expr of
      HsVar {}                 -> []
#if !MIN_VERSION_GHC(9,14)
      HsUnboundVar {}          -> []
#endif
#if !MIN_VERSION_GHC(9,12)
      HsRecSel {}              -> []
#endif
      HsOverLabel {}           -> []
      HsIPVar {}               -> []
      HsOverLit {}             -> []
      HsLit {}                 -> []
#if MIN_VERSION_GHC(9,10)
      HsLam _ _ matches        -> matchGroupConPats exprConPats matches
#else
      HsLam _ matches          -> matchGroupConPats exprConPats matches
      HsLamCase _ _ matches    -> matchGroupConPats exprConPats matches
#endif
      HsApp _ f e              -> exprConPats f ++ exprConPats e
#if MIN_VERSION_GHC(9,10)
      HsAppType _ e _          -> exprConPats e
#else
      HsAppType _ e _ _        -> exprConPats e
#endif
      OpApp _ l op r           -> exprConPats l ++ exprConPats op ++ exprConPats r
      NegApp _ e _             -> exprConPats e
#if MIN_VERSION_GHC(9,10)
      HsPar _ e                -> exprConPats e
#else
      HsPar _ _ e _            -> exprConPats e
#endif
      SectionL _ e op          -> exprConPats e ++ exprConPats op
      SectionR _ op e          -> exprConPats op ++ exprConPats e
      ExplicitTuple _ args _   -> concatMap tupArgConPats args
      ExplicitSum _ _ _ e      -> exprConPats e
      HsCase _ e matches       -> exprConPats e ++ matchGroupConPats exprConPats matches
      HsIf _ c t e             -> exprConPats c ++ exprConPats t ++ exprConPats e
      HsMultiIf _ grhss        -> concatMap (grhsConPats exprConPats) grhss
#if MIN_VERSION_GHC(9,10)
      HsLet _ binds e          -> localBindsConPats binds ++ exprConPats e
#else
      HsLet _ _ binds _ e      -> localBindsConPats binds ++ exprConPats e
#endif
      HsDo _ _ (L _ stmts)     -> concatMap (stmtConPats exprConPats) stmts
      ExplicitList _ es        -> concatMap exprConPats es
      RecordCon { rcon_flds = HsRecFields { rec_flds } }
                               -> concatMap fieldBindConPats rec_flds
      RecordUpd { rupd_expr, rupd_flds }
                               -> exprConPats rupd_expr ++ recUpdFieldsConPats rupd_flds
      HsGetField { gf_expr }   -> exprConPats gf_expr
      HsProjection {}          -> []
      ExprWithTySig _ e _      -> exprConPats e
      ArithSeq _ _ info        -> arithSeqInfoConPats info
      HsTypedBracket _ e       -> exprConPats e
      HsUntypedBracket _ quote -> quoteConPats quote
#if MIN_VERSION_GHC(9,14)
      HsTypedSplice _ (HsTypedSpliceExpr _ e)
                               -> exprConPats e
#else
      HsTypedSplice _ e        -> exprConPats e
#endif
      HsUntypedSplice _ splice -> untypedSpliceConPats splice
      HsProc _ p cmd           -> patConPats p ++ cmdTopConPats cmd
      HsStatic _ e             -> exprConPats e
      HsPragE _ _ e            -> exprConPats e
#if MIN_VERSION_GHC(9,10)
      HsEmbTy {}               -> []
#endif
#if MIN_VERSION_GHC(9,12)
      HsForAll _ _ e           -> exprConPats e
      HsQual _ (L _ context) e -> concatMap exprConPats context ++ exprConPats e
      HsFunArr _ _ a r         -> exprConPats a ++ exprConPats r
#endif
#if MIN_VERSION_GHC(9,14)
      HsHole {}                -> []
#endif
  where
    tupArgConPats (Present _ e) = exprConPats e
    tupArgConPats Missing {}    = []

    arithSeqInfoConPats (From e)              = exprConPats e
    arithSeqInfoConPats (FromThen e e')       = exprConPats e ++ exprConPats e'
    arithSeqInfoConPats (FromTo e e')         = exprConPats e ++ exprConPats e'
    arithSeqInfoConPats (FromThenTo e e' e'') = exprConPats e ++ exprConPats e' ++ exprConPats e''

#if MIN_VERSION_GHC(9,8)
    recUpdFieldsConPats RegularRecUpdFields { recUpdFields }      = concatMap fieldBindConPats recUpdFields
    recUpdFieldsConPats OverloadedRecUpdFields { olRecUpdFields } = concatMap fieldBindConPats olRecUpdFields
#else
    recUpdFieldsConPats = either (concatMap fieldBindConPats) (concatMap fieldBindConPats)
#endif


-- | Record fields in construction and update expressions.
--
fieldBindConPats :: GenLocated l (HsFieldBind lhs (LHsExpr GhcPs)) -> [Located RdrName]
fieldBindConPats (L _ HsFieldBind { hfbRHS }) = exprConPats hfbRHS


cmdTopConPats :: LHsCmdTop GhcPs -> [Located RdrName]
cmdTopConPats (L _ (HsCmdTop _ cmd)) = cmdConPats cmd


cmdConPats :: LHsCmd GhcPs -> [Located RdrName]
cmdConPats (L _ cmd) =
    case cmd of
      HsCmdArrApp _ f e _ _      -> exprConPats f ++ exprConPats e
#if MIN_VERSION_GHC(9,12)
      HsCmdArrForm _ op _ args   -> exprConPats op ++ concatMap cmdTopConPats args
#else
      HsCmdArrForm _ op _ _ args -> exprConPats op ++ concatMap cmdTopConPats args
#endif
      HsCmdApp _ c e             -> cmdConPats c ++ exprConPats e
#if MIN_VERSION_GHC(9,10)
      HsCmdLam _ _ matches       -> matchGroupConPats cmdConPats matches
      HsCmdPar _ c               -> cmdConPats c
#else
      HsCmdLam _ matches         -> matchGroupConPats cmdConPats matches
      HsCmdPar _ _ c _           -> cmdConPats c
      HsCmdLamCase _ _ matches   -> matchGroupConPats cmdConPats matches
#endif
      HsCmdCase _ e matches      -> exprConPats e ++ matchGroupConPats cmdConPats matches
      HsCmdIf _ _ c t e          -> exprConPats c ++ cmdConPats t ++ cmdConPats e
#if MIN_VERSION_GHC(9,10)
      HsCmdLet _ binds c         -> localBindsConPats binds ++ cmdConPats c
#else
      HsCmdLet _ _ binds _ c     -> localBindsConPats binds ++ cmdConPats c
#endif
      HsCmdDo _ (L _ stmts)      -> concatMap (stmtConPats cmdConPats) stmts


-- | Statements of @do@ blocks, comprehensions and guards, whose bodies are
-- expressions or commands.
--
stmtConPats :: (body -> [Located RdrName])
            -> LStmtLR GhcPs GhcPs body
            -> [Located RdrName]
stmtConPats bodyConPats (L _ stmt) =
    case stmt of
      LastStmt _ body _ _  -> bodyConPats body
      BindStmt _ p body    -> patConPats p ++ bodyConPats body
#if !MIN_VERSION_GHC(9,12)
      ApplicativeStmt {}   -> []
#endif
      BodyStmt _ body _ _  -> bodyConPats body
      LetStmt _ binds      -> localBindsConPats binds
      ParStmt _ blocks _ _ -> concatMap parStmtBlockConPats blocks
      TransStmt { trS_stmts, trS_using, trS_by }
                           -> concatMap (stmtConPats exprConPats) trS_stmts
                           ++ exprConPats trS_using
                           ++ concatMap exprConPats trS_by
      RecStmt { recS_stmts = L _ stmts }
                           -> concatMap (stmtConPats bodyConPats) stmts
  where
    parStmtBlockConPats :: ParStmtBlock GhcPs GhcPs -> [Located RdrName]
    parStmtBlockConPats (ParStmtBlock _ stmts _ _) =
      concatMap (stmtConPats exprConPats) stmts


localBindsConPats :: HsLocalBinds GhcPs -> [Located RdrName]
localBindsConPats binds =
    case binds of
      HsValBinds _ (ValBinds _ hsBinds _) -> concatMap (bindConPats . unLoc) hsBinds
      HsValBinds _ XValBindsLR {}         -> []
      HsIPBinds _ (IPBinds _ ipBinds)     -> concatMap (\(L _ (IPBind _ _ e)) -> exprConPats e) ipBinds
      EmptyLocalBinds _                   -> []


bindConPats :: HsBindLR GhcPs GhcPs -> [Located RdrName]
bindConPats bind =
    case bind of
      FunBind { fun_matches }      -> matchGroupConPats exprConPats fun_matches
      PatBind { pat_lhs, pat_rhs } -> patConPats pat_lhs ++ grhssConPats exprConPats pat_rhs
      VarBind { var_rhs }          -> exprConPats var_rhs
      PatSynBind _ PSB { psb_def, psb_dir }
                                   -> patConPats psb_def ++ patSynDirConPats psb_dir
  where
    patSynDirConPats Unidirectional             = []
    patSynDirConPats ImplicitBidirectional      = []
    patSynDirConPats (ExplicitBidirectional mg) = matchGroupConPats exprConPats mg


matchGroupConPats :: (body -> [Located RdrName])
                  -> MatchGroup GhcPs body
                  -> [Located RdrName]
matchGroupConPats bodyConPats MG { mg_alts = L _ matches } =
    concatMap (matchConPats bodyConPats) matches


matchConPats :: (body -> [Located RdrName])
             -> LMatch GhcPs body
             -> [Located RdrName]
matchConPats bodyConPats (L _ match) =
    concatMap patConPats (matchPats match) ++ grhssConPats bodyConPats (m_grhss match)


grhssConPats :: (body -> [Located RdrName])
             -> GRHSs GhcPs body
             -> [Located RdrName]
grhssConPats bodyConPats GRHSs { grhssGRHSs, grhssLocalBinds } =
    concatMap (grhsConPats bodyConPats) grhssGRHSs ++ localBindsConPats grhssLocalBinds


grhsConPats :: (body -> [Located RdrName])
            -> LGRHS GhcPs body
            -> [Located RdrName]
grhsConPats bodyConPats (L _ (GRHS _ guards body)) =
    concatMap (stmtConPats exprConPats) guards ++ bodyConPats body


-- | Pattern matches in declarations which are not tagged: annotations, rules,
-- splices and declaration quotes.
--
declConPats :: HsDecl GhcPs -> [Located RdrName]
declConPats decl =
    case decl of
      TyClD _ ClassDecl { tcdMeths } -> concatMap (bindConPats . unLoc) tcdMeths
      TyClD {}                       -> []
      InstD _ ClsInstD { cid_inst = ClsInstDecl { cid_binds } }
                                     -> concatMap (bindConPats . unLoc) cid_binds
      InstD {}                       -> []
      DerivD {}                      -> []
      ValD _ bind                    -> bindConPats bind
      SigD {}                        -> []
      KindSigD {}                    -> []
      DefD {}                        -> []
      ForD {}                        -> []
      WarningD {}                    -> []
      AnnD _ (HsAnnotation _ _ e)    -> exprConPats e
      RuleD _ HsRules { rds_rules }  -> concatMap ruleConPats rds_rules
      SpliceD _ (SpliceDecl _ (L _ splice) _)
                                     -> untypedSpliceConPats splice
      DocD {}                        -> []
      RoleAnnotD {}                  -> []
  where
    ruleConPats (L _ HsRule { rd_lhs, rd_rhs }) = exprConPats rd_lhs ++ exprConPats rd_rhs


quoteConPats :: HsQuote GhcPs -> [Located RdrName]
quoteConPats quote =
    case quote of
      ExpBr _ e      -> exprConPats e
      PatBr _ p      -> patConPats p
      DecBrL _ decls -> concatMap (declConPats . unLoc) decls
      DecBrG {}      -> []
      TypBr {}       -> []
      VarBr {}       -> []


untypedSpliceConPats :: HsUntypedSplice GhcPs -> [Located RdrName]
untypedSpliceConPats splice =
    case splice of
      HsUntypedSpliceExpr _ e -> exprConPats e
      HsQuasiQuote {}         -> []


-- | Patterns of a match.
--
matchPats :: Match GhcPs body -> [LPat GhcPs]
#if MIN_VERSION_GHC(9,12)
matchPats Match { m_pats = L _ pats } = pats
#else
matchPats Match { m_pats }            = m_pats
#endif


#if MIN_VERSION_GHC(9,10)
unSpanAnn :: HasLoc a => GenLocated a e -> GenLocated SrcSpan e
#else
unSpanAnn :: GenLocated (SrcSpanAnn' x) RdrName -> Located RdrName
#endif
unSpanAnn (L s a) = L (locA s) a

#if MIN_VERSION_GHC(9,10)
locAnn :: SrcSpanAnnA -> SrcSpan
#else
locAnn :: SrcSpanAnn' a -> SrcSpan
#endif
locAnn = locA

hsSigTypeToHsType :: HsSigType GhcPs -> HsType GhcPs
hsSigTypeToHsType = unLoc . sig_body

--
--
--

famResultKindSignature :: FamilyResultSig GhcPs
                       -> Maybe (Either (HsKind GhcPs) GhcPsHsTyVarBndr)
famResultKindSignature (NoSig _)           = Nothing
famResultKindSignature (KindSig _ ki)      = Just (Left (unLoc ki))
famResultKindSignature (TyVarSig _ bndr)   = Just (Right (unLoc bndr))
