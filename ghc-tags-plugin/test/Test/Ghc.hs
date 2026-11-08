{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ghc (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (toList)
import Data.List (sort, groupBy, isPrefixOf, isInfixOf, nub)
import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan (..), unLoc, srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import qualified GHC.Utils.Outputable as Out
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import GhcTags.Ghc
import Test.Ghc.Generators


tests :: TestTree
tests = testGroup "Ghc"
    [ testProperty "pattern signature filtering follows the option within each binding group" patternSignatureProp
    , testProperty "method visibility follows class exports, instances and lexical scope" methodExportsProp
    , testProperty "family tags follow declarations and exports; instances are exported" familyExportsProp
    , testProperty "pattern-bound tags preserve locations, scope and filtering" patternBindingsProp
    , testProperty "pattern-match tags name matched constructors once per line" patternMatchesProp
    , testProperty "pattern-match tags leave declaration tags unchanged" patternMatchDeclarationsProp
    ]

instance Arbitrary Filter where
  arbitrary = elements [RetainAll, RemoveTerms]
  shrink RetainAll   = [RemoveTerms]
  shrink RemoveTerms = []
  

patternSignatureProp :: Filter -> [ArbHsDecl] -> Property
patternSignatureProp filterBindings input =
    let declarations = map getArbHsDecl input
        tags = hsDeclsToGhcTags filterBindings ExcludePatternMatches Nothing declarations
        actual =
          [ (BSC.unpack gtTag, kind, signatureType)
          | GhcTag {gtTag, gtKind} <- tags
          , let (kind, signatureType) = case gtKind of
                  GtkTypeSignature ty ->
                    ("signature", Out.renderWithContext Out.defaultSDocContext $ Out.ppr ty)
                  GtkPatternSynonym -> ("pattern", "")
                  _ -> ("unexpected", "")
          ]
        declarationsModel = concatMap (\decl -> case unLoc decl of
          SigD _ (PatSynSig _ names signature) ->
            [ (occNameString $ rdrNameOcc $ unLoc name, "signature",
               Out.renderWithContext Out.defaultSDocContext $ Out.ppr signature)
            | name <- names
            ]
          ValD _ (PatSynBind _ PSB {psb_id}) ->
            [(occNameString $ rdrNameOcc $ unLoc psb_id, "pattern", "")]
          _ -> []) declarations
        signatures = [name | (name, "signature", _) <- declarationsModel]
        definitions = [name | (name, "pattern", _) <- declarationsModel]
        expected =
          filter
            (\(name, kind, _) -> filterBindings == RetainAll
                              || kind /= "pattern"
                              || name `notElem` signatures)
          declarationsModel
    in checkCoverage
       $ cover 25 (filterBindings == RemoveTerms) "filtering enabled"
       $ cover 25 (filterBindings == RetainAll) "filtering disabled"
       $ cover 30 (not (null signatures) && not (null definitions)) "signatures and definitions"
       $ cover 20 (any (`elem` definitions) signatures) "matching signature and definition names"
       $ cover 20 (any (`notElem` signatures) definitions) "unsigned pattern definitions"
       $ actual === expected


methodExportsProp :: ArbClassModule -> Property
methodExportsProp (ArbClassModule ast@(L _ HsModule {hsmodDecls, hsmodExports})) =
    let nameOf = occNameString . rdrNameOcc . unLoc
        wrappedName = occNameString . rdrNameOcc . ieWrappedName . unLoc
        classes =
          [ (nameOf tcdLName, map nameOf $ concatMap (signatureNames . unLoc) tcdSigs)
          | L _ (TyClD _ ClassDecl {tcdLName, tcdSigs}) <- hsmodDecls
          ]
        membersOf className = concat [names | (name, names) <- classes, name == className]
        exportedMembers :: IE GhcPs -> [String]
        exportedMembers ie = case ie of
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
          IEVar _ name _ -> [wrappedName name]
          IEThingAll _ name _ -> membersOf (wrappedName name)
          IEThingWith _ name wildcard children _ ->
#else
          IEVar _ name -> [wrappedName name]
          IEThingAll _ name -> membersOf (wrappedName name)
          IEThingWith _ name wildcard children ->
#endif
            case wildcard of
              IEWildcard {} -> membersOf (wrappedName name)
              NoIEWildcard -> filter (`elem` map wrappedName children) (membersOf $ wrappedName name)
          _ -> []
        publicNames = case hsmodExports of
          Nothing -> concatMap snd classes
          Just exports -> concatMap (exportedMembers . unLoc) (unLoc exports)
        classNames = concatMap (\decl -> case unLoc decl of
          TyClD _ ClassDecl {tcdSigs, tcdMeths} ->
               concatMap (signatureNames . unLoc) tcdSigs
            ++ [fun_id | L _ FunBind {fun_id} <- toList tcdMeths]
          _ -> []) hsmodDecls
        -- instances are exported whatever the export list says
        instanceNames = concatMap (\decl -> case unLoc decl of
          InstD _ (ClsInstD _ ClsInstDecl {cid_sigs, cid_binds}) ->
               concatMap (signatureNames . unLoc) cid_sigs
            ++ [fun_id | L _ FunBind {fun_id} <- toList cid_binds]
          _ -> []) hsmodDecls
        directNames = classNames ++ instanceNames
        publicLocations =  [getLocA name | name <- classNames, nameOf name `elem` publicNames]
                        ++ map getLocA instanceNames
        directLocations = map getLocA directNames
        tags = filter (\tag -> case gtKind tag of
                         GtkModule -> False
                         GtkTypeClass -> False
                         GtkTypeClassInstance {} -> False
                         _ -> True) (getGhcTags RetainAll ExcludePatternMatches ast)
        expected = [gtSrcSpan tag `elem` publicLocations | tag <- tags]
        shadowsExport = any (\tag -> BSC.unpack (gtTag tag) `elem` publicNames
                                 && gtSrcSpan tag `notElem` directLocations) tags
    in checkCoverage
       $ cover 20 (not $ null publicLocations) "exported methods"
       $ cover 20 (any ((`notElem` publicNames) . nameOf) classNames) "hidden methods"
       $ cover 20 (any ((`notElem` publicNames) . nameOf) instanceNames) "instances of hidden methods"
       $ cover 10 shadowsExport "local bindings shadow exported methods"
       $ counterexample (show [(BSC.unpack $ gtTag tag, gtSrcSpan tag) | tag <- tags])
       $ map gtIsExported tags === expected
  where
    signatureNames :: Sig GhcPs -> [LIdP GhcPs]
    signatureNames (ClassOpSig _ _ names _) = names
    signatureNames (TypeSig _ names _) = names
    signatureNames _ = []


familyExportsProp :: ArbFamilyModule -> Property
familyExportsProp (ArbFamilyModule ast@(L _ HsModule {hsmodDecls, hsmodExports})) =
    let nameOf = occNameString . rdrNameOcc . unLoc
        wrappedName = occNameString . rdrNameOcc . ieWrappedName . unLoc
        typeInstanceName :: TyFamInstDecl GhcPs -> LIdP GhcPs
        typeInstanceName TyFamInstDecl {tfid_eqn = FamEqn {feqn_tycon}} = feqn_tycon
        dataInstanceNames :: DataFamInstDecl GhcPs -> ([LIdP GhcPs], [LIdP GhcPs], [(String, [String])])
        dataInstanceNames DataFamInstDecl {dfid_eqn = FamEqn {feqn_tycon, feqn_rhs = HsDataDefn {dd_cons}}} =
          let children = concatMap (constructorNames . unLoc) dd_cons
          in (children, [feqn_tycon], [(nameOf feqn_tycon, map nameOf children)])
        -- names, instance names (exported whatever the export list says)
        -- and parent-children edges
        declarations = map (\decl -> case unLoc decl of
          TyClD _ (FamDecl _ FamilyDecl {fdLName}) -> ([fdLName], [], [])
          TyClD _ ClassDecl {tcdLName, tcdATs, tcdATDefs} ->
            let members = [fdLName | L _ FamilyDecl {fdLName} <- tcdATs]
            in (members ++ map (typeInstanceName . unLoc) tcdATDefs, [],
                [(nameOf tcdLName, map nameOf members)])
          InstD _ (TyFamInstD _ decl') -> ([], [typeInstanceName decl'], [])
          InstD _ (DataFamInstD _ decl') -> dataInstanceNames decl'
          InstD _ (ClsInstD _ ClsInstDecl {cid_tyfam_insts, cid_datafam_insts}) ->
            let dataInstances = map (dataInstanceNames . unLoc) cid_datafam_insts
            in ( concat [children | (children, _, _) <- dataInstances]
               , map (typeInstanceName . unLoc) cid_tyfam_insts
                 ++ concat [heads | (_, heads, _) <- dataInstances]
               , concat [edges | (_, _, edges) <- dataInstances])
          _ -> ([], [], [])) hsmodDecls
        names = concat [names' | (names', _, _) <- declarations]
        instanceNames = concat [heads | (_, heads, _) <- declarations]
        childrenOf parent = concat [children | (_, _, edges) <- declarations,
                                               (name, children) <- edges, name == parent]
        expandExport :: IE GhcPs -> [String]
        expandExport ie = case ie of
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
          IEVar _ name _ -> [wrappedName name]
          IEThingAbs _ name _ -> [wrappedName name]
          IEThingAll _ name _ -> wrappedName name : childrenOf (wrappedName name)
          IEThingWith _ name wildcard children _ ->
#else
          IEVar _ name -> [wrappedName name]
          IEThingAbs _ name -> [wrappedName name]
          IEThingAll _ name -> wrappedName name : childrenOf (wrappedName name)
          IEThingWith _ name wildcard children ->
#endif
            wrappedName name : case wildcard of
              IEWildcard {} -> childrenOf (wrappedName name)
              NoIEWildcard -> map wrappedName children
          _ -> []
        publicNames = case hsmodExports of
          Nothing -> map nameOf names
          Just exports -> concatMap (expandExport . unLoc) (unLoc exports)
        expected = sort $  [(nameOf name, show $ getLocA name, nameOf name `elem` publicNames) | name <- names]
                        ++ [(nameOf name, show $ getLocA name, True) | name <- instanceNames]
        tags = filter (\tag -> case gtKind tag of
                         GtkModule -> False
                         GtkTypeClass -> False
                         GtkTypeClassInstance {} -> False
                         _ -> True) (getGhcTags RetainAll ExcludePatternMatches ast)
        actual = sort [(BSC.unpack gtTag, show gtSrcSpan, gtIsExported) | GhcTag {gtTag, gtSrcSpan, gtIsExported} <- tags]
        hasDefault = or [not (null tcdATDefs) | L _ (TyClD _ ClassDecl {tcdATDefs}) <- hsmodDecls]
        hiddenChildren = or [parent `elem` publicNames && any (`notElem` publicNames) children
                            | (_, _, edges) <- declarations, (parent, children) <- edges]
    in checkCoverage
       $ cover 20 (any ((`elem` publicNames) . nameOf) names) "exported families or members"
       $ cover 10 (any ((`notElem` publicNames) . nameOf) names) "hidden families or members"
       $ cover 10 hasDefault "associated type defaults"
       $ cover 10 hiddenChildren "exported parents with hidden members"
       $ cover 10 (any ((`notElem` publicNames) . nameOf) instanceNames) "instances of hidden families"
       $ actual === expected
  where
    constructorNames :: ConDecl GhcPs -> [LIdP GhcPs]
    constructorNames ConDeclH98 {con_name, con_args} =
      con_name : case con_args of
        RecCon (L _ fields) ->
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
          [foLabel field | L _ HsConDeclRecField {cdrf_names} <- fields, L _ field <- cdrf_names]
#else
          [foLabel field | L _ ConDeclField {cd_fld_names} <- fields, L _ field <- cd_fld_names]
#endif
        _ -> []
    constructorNames _ = []


patternBindingsProp :: Filter -> ArbPatternModule -> Property
patternBindingsProp filtering (ArbPatternModule source ast) =
    let -- Generated binders start with "bound"; constructors, field references,
        -- view expressions and type variables do not. Build the model from
        -- source tokens rather than duplicating the AST traversal under test.
        tokens :: String -> [(String, Int)]
        tokens line =
          [ (map snd token, column)
          | token@((column, _) : _) <- groupBy (\(_, a) (_, b) -> identifierChar a == identifierChar b) (zip [1 ..] line)
          , "bound" `isPrefixOf` map snd token
          ]
        identifierChar c = isAlphaNum c || c == '_'
        declarations =
          [ (name, kind, (lineNumber, column, lineNumber, column + length name), topLevel)
          | (lineNumber, line@(first : _)) <- zip [1 ..] (lines source)
          , lineNumber > 1
          , let topLevel = not (isSpace first)
                kind = if "bound" `isPrefixOf` dropWhile isSpace line then "signature" else "term"
          , (name, column) <- tokens line
          ]
        signedNames = [(name, topLevel) | (name, "signature", _, topLevel) <- declarations]
        exportNames = [name | header <- take 1 (lines source), (name, _) <- tokens header]
        exportsAll = case unLoc ast of
          HsModule {hsmodExports = Nothing} -> True
          _ -> False
        expected = sort
          [ (name, kind, location, topLevel && (exportsAll || name `elem` exportNames))
          | (name, kind, location, topLevel) <- declarations
          , filtering == RetainAll || kind == "signature" || (name, topLevel) `notElem` signedNames
          ]
        actual = sort
          [ (BSC.unpack gtTag, kind, location, gtIsExported)
          | GhcTag {gtTag, gtKind, gtSrcSpan, gtIsExported} <- getGhcTags filtering ExcludePatternMatches ast
          , let kind = case gtKind of
                  GtkTerm -> "term"
                  GtkTypeSignature {} -> "signature"
                  _ -> "unexpected"
          , case gtKind of GtkModule -> False; _ -> True
          , let location = case gtSrcSpan of
                  RealSrcSpan span_ _ -> (srcSpanStartLine span_, srcSpanStartCol span_, srcSpanEndLine span_, srcSpanEndCol span_)
                  UnhelpfulSpan {} -> (-1, -1, -1, -1)
          ]
        signedTerms = any (\(name, kind, _, topLevel) -> kind == "term" && (name, topLevel) `elem` signedNames) declarations
        localTerms = any (\(_, kind, _, topLevel) -> kind == "term" && not topLevel) declarations
    in checkCoverage
       $ cover 25 (filtering == RetainAll) "retain definitions"
       $ cover 25 (filtering == RemoveTerms) "filter signed definitions"
       $ cover 10 signedTerms "signed pattern bindings"
       $ cover 10 localTerms "local pattern bindings"
       $ cover 3 ("referenceField" `isInfixOf` source) "record patterns with puns"
       $ cover 3 ("reference -> reference" `isInfixOf` source) "view patterns"
       $ actual === expected


patternMatchesProp :: ArbMatchModule -> Property
patternMatchesProp ArbMatchModule {matchModuleSource, matchModuleAst} =
    let -- Generated constructors matched by patterns are the only tokens
        -- starting with "Matched". Build the model from source tokens rather
        -- than duplicating the AST traversal under test.
        matched :: String -> [String]
        matched line =
          [ token
          | token <- words [if isAlphaNum c || c == '_' then c else ' ' | c <- line]
          , "Matched" `isPrefixOf` token
          ]
        sourceLines = lines matchModuleSource
        expected = sort $ nub
          [ (name, lineNumber, "match")
          | (lineNumber, line) <- zip [1 ..] sourceLines
          , name <- matched line
          ]
        -- Pattern matches are neither local nor file-local, even in local
        -- bindings or instance methods.
        actual = sort
          [ (BSC.unpack gtTag, lineNumber, kind)
          | GhcTag {gtTag, gtKind = GtkPatternMatch, gtSrcSpan, gtIsExported, gtIsLocal}
              <- getGhcTags RetainAll IncludePatternMatches matchModuleAst
          , let kind = if gtIsExported && not gtIsLocal then "match" else "local match"
                lineNumber = case gtSrcSpan of
                  RealSrcSpan span_ _ -> srcSpanStartLine span_
                  UnhelpfulSpan {} -> -1
          ]
        repeated = any (\line -> let names = matched line in names /= nub names) sourceLines
        continued = any (\line -> take 1 line == " " && not (null (matched line))) sourceLines
    in -- Each syntactic context is rare, so run more tests.
       withMaxSuccess 2000
       $ checkCoverage
       $ cover 20 repeated "repeated matches on a line"
       $ cover 20 continued "patterns spanning lines"
       $ cover 20 (any (`isInfixOf` matchModuleSource) ["[]", "()", " : "]) "built-in syntax"
       $ actual === expected


patternMatchDeclarationsProp :: Filter -> ArbMatchModule -> Property
patternMatchDeclarationsProp filtering ArbMatchModule {matchModuleAst} =
    let declarations patternMatches =
          [ (BSC.unpack gtTag, gtSrcSpan, kindName gtKind, gtIsExported, gtIsLocal, gtFFI)
          | GhcTag {gtTag, gtSrcSpan, gtKind, gtIsExported, gtIsLocal, gtFFI}
              <- getGhcTags filtering patternMatches matchModuleAst
          , case gtKind of GtkPatternMatch -> False; _ -> True
          ]
        excluded = declarations ExcludePatternMatches
    in checkCoverage
       $ cover 20 (any (\(_, _, _, _, local, _) -> local) excluded) "local bindings"
       $ cover 10 (any (\(_, _, kind, _, _, _) -> kind == "instance member") excluded) "instance methods"
       $ declarations IncludePatternMatches === excluded


-- | The constructor of a tag kind, without the syntax it carries.
kindName :: GhcTagKind -> String
kindName kind = case kind of
    GtkModule                     -> "module"
    GtkTerm                       -> "term"
    GtkFunction                   -> "function"
    GtkTypeConstructor {}         -> "type constructor"
    GtkDataConstructor {}         -> "data constructor"
    GtkGADTConstructor {}         -> "GADT constructor"
    GtkRecordField                -> "record field"
    GtkTypeSynonym {}             -> "type synonym"
    GtkTypeSignature {}           -> "type signature"
    GtkTypeKindSignature {}       -> "kind signature"
    GtkPatternSynonym             -> "pattern synonym"
    GtkTypeClass                  -> "class"
    GtkTypeClassMember {}         -> "class member"
    GtkTypeClassInstance {}       -> "instance"
    GtkTypeClassInstanceMember {} -> "instance member"
    GtkTypeFamily {}              -> "type family"
    GtkTypeFamilyInstance {}      -> "type family instance"
    GtkDataTypeFamily {}          -> "data family"
    GtkDataTypeFamilyInstance {}  -> "data family instance"
    GtkForeignImport              -> "foreign import"
    GtkForeignExport              -> "foreign export"
    GtkPatternMatch               -> "pattern match"
