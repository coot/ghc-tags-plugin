{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Test.Ghc (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (toList)
import Data.List (sort)
import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import qualified GHC.Utils.Outputable as Out
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import GhcTags.Ghc
import Test.Ghc.Generators


tests :: TestTree
tests = testGroup "Ghc"
    [ testProperty "pattern signatures replace definitions in the same binding group" patternSignatureProp
    , testProperty "method visibility follows class exports and lexical scope" methodExportsProp
    , testProperty "family tags follow declarations and exports" familyExportsProp
    ]


patternSignatureProp :: [ArbHsDecl] -> Property
patternSignatureProp input =
    let declarations = map getArbHsDecl input
        tags = hsDeclsToGhcTags Nothing declarations
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
        expected = filter
          (\(name, kind, _) -> kind /= "pattern" || name `notElem` signatures)
          declarationsModel
    in checkCoverage
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
        directNames = concatMap (\decl -> case unLoc decl of
          TyClD _ ClassDecl {tcdSigs, tcdMeths} ->
               concatMap (signatureNames . unLoc) tcdSigs
            ++ [fun_id | L _ FunBind {fun_id} <- toList tcdMeths]
          InstD _ (ClsInstD _ ClsInstDecl {cid_sigs, cid_binds}) ->
               concatMap (signatureNames . unLoc) cid_sigs
            ++ [fun_id | L _ FunBind {fun_id} <- toList cid_binds]
          _ -> []) hsmodDecls
        publicLocations = [getLocA name | name <- directNames, nameOf name `elem` publicNames]
        directLocations = map getLocA directNames
        tags = filter (\tag -> case gtKind tag of
                         GtkModule -> False
                         GtkTypeClass -> False
                         GtkTypeClassInstance {} -> False
                         _ -> True) (getGhcTags ast)
        expected = [gtSrcSpan tag `elem` publicLocations | tag <- tags]
        shadowsExport = any (\tag -> BSC.unpack (gtTag tag) `elem` publicNames
                                 && gtSrcSpan tag `notElem` directLocations) tags
    in checkCoverage
       $ cover 20 (not $ null publicLocations) "exported methods"
       $ cover 20 (any ((`notElem` publicNames) . nameOf) directNames) "hidden methods"
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
        dataInstanceNames :: DataFamInstDecl GhcPs -> ([LIdP GhcPs], [(String, [String])])
        dataInstanceNames DataFamInstDecl {dfid_eqn = FamEqn {feqn_tycon, feqn_rhs = HsDataDefn {dd_cons}}} =
          let children = concatMap (constructorNames . unLoc) dd_cons
          in (feqn_tycon : children, [(nameOf feqn_tycon, map nameOf children)])
        declarations = map (\decl -> case unLoc decl of
          TyClD _ (FamDecl _ FamilyDecl {fdLName}) -> ([fdLName], [])
          TyClD _ ClassDecl {tcdLName, tcdATs, tcdATDefs} ->
            let members = [fdLName | L _ FamilyDecl {fdLName} <- tcdATs]
            in (members ++ map (typeInstanceName . unLoc) tcdATDefs,
                [(nameOf tcdLName, map nameOf members)])
          InstD _ (TyFamInstD _ decl') -> ([typeInstanceName decl'], [])
          InstD _ (DataFamInstD _ decl') -> dataInstanceNames decl'
          InstD _ (ClsInstD _ ClsInstDecl {cid_tyfam_insts, cid_datafam_insts}) ->
            let dataInstances = map (dataInstanceNames . unLoc) cid_datafam_insts
            in (map (typeInstanceName . unLoc) cid_tyfam_insts ++ concatMap fst dataInstances,
                concatMap snd dataInstances)
          _ -> ([], [])) hsmodDecls
        names = concatMap fst declarations
        childrenOf parent = concat [children | (_, edges) <- declarations,
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
        expected = sort [(nameOf name, show $ getLocA name, nameOf name `elem` publicNames) | name <- names]
        tags = filter (\tag -> case gtKind tag of
                         GtkModule -> False
                         GtkTypeClass -> False
                         GtkTypeClassInstance {} -> False
                         _ -> True) (getGhcTags ast)
        actual = sort [(BSC.unpack gtTag, show gtSrcSpan, gtIsExported) | GhcTag {gtTag, gtSrcSpan, gtIsExported} <- tags]
        hasDefault = or [not (null tcdATDefs) | L _ (TyClD _ ClassDecl {tcdATDefs}) <- hsmodDecls]
        hiddenChildren = or [parent `elem` publicNames && any (`notElem` publicNames) children
                            | (_, edges) <- declarations, (parent, children) <- edges]
    in checkCoverage
       $ cover 20 (any ((`elem` publicNames) . nameOf) names) "exported families or members"
       $ cover 10 (any ((`notElem` publicNames) . nameOf) names) "hidden families or members"
       $ cover 10 hasDefault "associated type defaults"
       $ cover 10 hiddenChildren "exported parents with hidden members"
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
