{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Test.Ghc (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (toList)
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
