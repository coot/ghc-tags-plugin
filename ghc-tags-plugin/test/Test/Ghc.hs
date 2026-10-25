{-# LANGUAGE NamedFieldPuns  #-}

module Test.Ghc (tests) where

import qualified Data.ByteString.Char8 as BSC
import GHC.Hs (HsDecl (..), Sig (..), HsBindLR (..), PatSynBind (..))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.Utils.Outputable as Out
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import GhcTags.Ghc
import Test.Ghc.Generators


tests :: TestTree
tests = testGroup "Ghc"
    [ testProperty "pattern signatures and definitions have distinct kinds" patternSignatureProp ]


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
        expected = concatMap (\decl -> case unLoc decl of
          SigD _ (PatSynSig _ names signature) ->
            [ (occNameString $ rdrNameOcc $ unLoc name, "signature",
               Out.renderWithContext Out.defaultSDocContext $ Out.ppr signature)
            | name <- names
            ]
          ValD _ (PatSynBind _ PSB {psb_id}) ->
            [(occNameString $ rdrNameOcc $ unLoc psb_id, "pattern", "")]
          _ -> []) declarations
        signatures = [name | (name, "signature", _) <- expected]
        definitions = [name | (name, "pattern", _) <- expected]
    in checkCoverage
       $ cover 30 (not (null signatures) && not (null definitions)) "signatures and definitions"
       $ cover 20 (any (`elem` definitions) signatures) "matching signature and definition names"
       $ actual === expected
