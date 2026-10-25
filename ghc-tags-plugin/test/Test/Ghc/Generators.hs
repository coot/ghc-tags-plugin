{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Ghc.Generators (ArbHsDecl (..), ArbClassModule (..), ArbFamilyModule (..)) where

import Data.Char (isUpper)
import Data.List (intercalate)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (TypeFamilies, ExplicitNamespaces))
import qualified GHC.Parser as Parser
import GHC.Parser.Lexer (ParseResult (..), initParserState, mkParserOpts, unP)
import GHC.Types.Name.Occurrence (mkDataOcc, mkOccName, mkTcOcc, occNameSpace, occNameString)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), Located, mkRealSrcLoc, unLoc)
#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
import GHC.Utils.Error (emptyDiagOpts)
#else
import GHC.Utils.Error (DiagOpts (..))
#endif
import qualified GHC.Utils.Outputable as Out
import Test.Tasty.QuickCheck


newtype ArbHsDecl = ArbHsDecl { getArbHsDecl :: LHsDecl GhcPs }

instance Show ArbHsDecl where
    show = Out.renderWithContext Out.defaultSDocContext . Out.ppr . getArbHsDecl

instance Arbitrary ArbHsDecl where
    arbitrary = oneof
      [ patternSignature <$> arbitrary <*> arbitrary
      , patternDefinition <$> arbitrary
      ]
    shrink (ArbHsDecl decl) = map ArbHsDecl $ case unLoc decl of
      SigD ext sig -> [fmap (const $ SigD ext smaller) decl | smaller <- shrinkSignature sig]
      ValD ext bind -> [fmap (const $ ValD ext smaller) decl | smaller <- shrinkBinding bind]
      _ -> []


newtype ConstructorName = ConstructorName String
  deriving Show

instance Arbitrary ConstructorName where
    arbitrary = ConstructorName <$> frequency
      [ (3, elements ["Done", "Yield", "Await"])
      , (1, fixConstructorName <$>
          listOf (elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_")))
      ]
    shrink (ConstructorName name) =
      [ ConstructorName smaller
      | smaller <- map fixConstructorName (shrink name)
      , (length smaller, smaller) < (length name, name)
      ]


fixConstructorName :: String -> String
fixConstructorName name =
    let valid = filter (`elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_")) name
    in case valid of
      first : _ | isUpper first -> valid
      _ -> 'P' : valid


patternSignature :: ConstructorName -> ConstructorName -> ArbHsDecl
patternSignature (ConstructorName name) (ConstructorName typeName) =
    let readerName = noLocA (mkRdrUnqual $ mkDataOcc name)
        hsType = noLocA (HsTyVar noAnn NotPromoted
                          (noLocA $ mkRdrUnqual $ mkTcOcc typeName))
    in ArbHsDecl $ noLocA $ SigD noExtField $
         PatSynSig noAnn [readerName] (noLocA $ mkHsImplicitSigType hsType)


patternDefinition :: ConstructorName -> ArbHsDecl
patternDefinition (ConstructorName name) =
    ArbHsDecl $ noLocA $ ValD noExtField $ PatSynBind noExtField PSB
      { psb_ext = noAnn
      , psb_id = noLocA (mkRdrUnqual $ mkDataOcc name)
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
      , psb_args = PrefixCon []
#else
      , psb_args = PrefixCon [] []
#endif
      , psb_def = noLocA (WildPat noExtField)
      , psb_dir = Unidirectional
      }


shrinkSignature :: Sig GhcPs -> [Sig GhcPs]
shrinkSignature (PatSynSig ext names sigType) =
     [PatSynSig ext names' sigType | names' <- shrinkList shrinkName names, not (null names')]
  ++ [PatSynSig ext names sigType' | sigType' <- shrinkSignatureType sigType]
shrinkSignature _ = []


shrinkSignatureType :: LHsSigType GhcPs -> [LHsSigType GhcPs]
shrinkSignatureType (L loc sig@HsSig {sig_body = L bodyLoc (HsTyVar ext promotion name)}) =
    [ L loc sig {sig_body = L bodyLoc (HsTyVar ext promotion smaller)}
    | smaller <- shrinkName name
    ]
shrinkSignatureType _ = []


shrinkBinding :: HsBind GhcPs -> [HsBind GhcPs]
shrinkBinding (PatSynBind ext psb@PSB {psb_id}) =
    [PatSynBind ext psb {psb_id = smaller} | smaller <- shrinkName psb_id]
shrinkBinding _ = []


shrinkName :: GenLocated loc RdrName -> [GenLocated loc RdrName]
shrinkName name =
    let occurrence = rdrNameOcc (unLoc name)
    in [ fmap (const $ mkRdrUnqual $ mkOccName (occNameSpace occurrence) smaller) name
       | ConstructorName smaller <- shrink (ConstructorName $ occNameString occurrence)
       ]


-- Parsed modules keep real source locations, including distinct locations for
-- local bindings which shadow class methods.
newtype ArbClassModule = ArbClassModule { getArbClassModule :: Located (HsModule GhcPs) }

instance Show ArbClassModule where
    show = Out.renderWithContext Out.defaultSDocContext . Out.ppr . getArbClassModule

instance Arbitrary ArbClassModule where
    arbitrary = do
      ConstructorName classSuffix <- arbitrary
      methodSuffixes <- resize 5 arbitrary
      let className = "Class" ++ classSuffix
          methods = ["method" ++ name ++ show i | (i, ConstructorName name) <- zip [0 :: Int ..] methodSuffixes]
      selected <- sublistOf methods
      exports <- elements
        [ ""
        , "()"
        , "(" ++ className ++ "(..))"
        , "(" ++ className ++ "(" ++ intercalate "," selected ++ "))"
        , "(" ++ intercalate "," selected ++ ")"
        , "(" ++ className ++ ")"
        , "(Eq(..))"
        ]
      defaults <- mapM methodBinding methods
      instances <- mapM (\name -> do
        signature <- elements ["", "  " ++ name ++ " :: Int -> ()\n"]
        binding <- methodBinding name
        pure (signature ++ binding)) methods
      pure $ ArbClassModule $ parseModule $
           "module Example " ++ exports ++ " where\n"
        ++ "class " ++ className ++ " a where\n"
        ++ concat ["  " ++ name ++ " :: a -> ()\n" | name <- methods]
        ++ concat defaults
        ++ "instance " ++ className ++ " Int where\n"
        ++ concat instances
    shrink (ArbClassModule ast@(L _ HsModule {hsmodExports})) =
      [ ArbClassModule (fmap (\mod_ -> mod_ {hsmodExports = smaller}) ast)
      | smaller <- case hsmodExports of
          Nothing -> [Just $ noLocA []]
          Just exports ->
            [Just (fmap (const entries) exports) | entries <- shrinkList (const []) (unLoc exports)]
      ]


methodBinding :: String -> Gen String
methodBinding name = do
    helper <- elements [name, "local" ++ name]
    signature <- elements ["", "      " ++ helper ++ " :: b -> ()\n"]
    pure $ "  " ++ name ++ " x = " ++ helper ++ " x\n"
        ++ "    where\n"
        ++ signature
        ++ "      " ++ helper ++ " _ = ()\n"


#if !MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
emptyDiagOpts :: DiagOpts
emptyDiagOpts = DiagOpts
  { diag_warning_flags       = EnumSet.empty
  , diag_fatal_warning_flags = EnumSet.empty
  , diag_warn_is_error       = False
  , diag_reverse_errors      = False
  , diag_max_errors          = Nothing
  , diag_ppr_ctx             = Out.defaultSDocContext
  }
#endif


parseModule :: String -> Located (HsModule GhcPs)
parseModule source =
    case unP Parser.parseModule (initParserState options (stringToStringBuffer source) location) of
      POk _ ast -> ast
      PFailed _ -> error ("Invalid generated module:\n" ++ source)
  where
    location = mkRealSrcLoc (mkFastString "Example.hs") 1 1
    options = mkParserOpts (EnumSet.fromList [TypeFamilies, ExplicitNamespaces]) emptyDiagOpts
#if !MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
                []
#endif
                False False False False


newtype ArbFamilyModule = ArbFamilyModule { getArbFamilyModule :: Located (HsModule GhcPs) }

instance Show ArbFamilyModule where
    show = Out.renderWithContext Out.defaultSDocContext . Out.ppr . getArbFamilyModule

instance Arbitrary ArbFamilyModule where
    arbitrary = do
      ConstructorName suffix <- arbitrary
      familySuffixes <- resize 4 arbitrary
      let className = "Class" ++ suffix
      families <- mapM (\(i, ConstructorName name) -> do
        associated <- arbitrary
        withDefault <- arbitrary
        record <- arbitrary
        rhs <- elements ["Maybe a", "[a]", "(a, a)", "a"]
        let ty = "Type" ++ name ++ show i
            dat = "Data" ++ name ++ show i
            con = "Mk" ++ dat
            field = "field" ++ dat
            declaration = ["type " ++ ty ++ " a", "data " ++ dat ++ " a"]
            def = ["type " ++ ty ++ " a = " ++ rhs | withDefault]
            equations = [ "type " ++ ty ++ " Int = Bool"
                        , "data " ++ dat ++ " Int = " ++ con
                            ++ if record then " { " ++ field ++ " :: Int }" else " Int" ]
            standalone = ["type family " ++ ty ++ " a", "data family " ++ dat ++ " a",
                          "type instance " ++ ty ++ " Int = Bool",
                          "data instance " ++ dat ++ " Int = " ++ con
                            ++ if record then " { " ++ field ++ " :: Int }" else " Int"]
            exports = [ty, "type " ++ ty, dat, dat ++ "(..)", dat ++ "(" ++ con ++ ")"]
                   ++ [field | record]
                   ++ [dat ++ "(" ++ field ++ ")" | record]
                   ++ [className ++ "(" ++ member ++ ")" | associated, member <- [ty, dat]]
        pure (if associated then ([], declaration ++ def, equations, exports)
                            else (standalone, [], [], exports)))
        (zip [0 :: Int ..] familySuffixes)
      exports <- sublistOf ([className, className ++ "(..)", "Eq(..)"]
                        ++ concat [es | (_, _, _, es) <- families])
      exportList <- frequency [(1, pure ""), (4, pure $ "(" ++ intercalate "," exports ++ ")")]
      pure $ ArbFamilyModule $ parseModule $
           "module Example " ++ exportList ++ " where\n"
        ++ unlines (concat [ds | (ds, _, _, _) <- families])
        ++ "class " ++ className ++ " a where\n"
        ++ unlines ["  " ++ d | (_, ds, _, _) <- families, d <- ds]
        ++ "instance " ++ className ++ " Int where\n"
        ++ unlines ["  " ++ d | (_, _, ds, _) <- families, d <- ds]
    shrink (ArbFamilyModule ast@(L _ HsModule {hsmodExports})) =
      [ ArbFamilyModule (fmap (\mod_ -> mod_ {hsmodExports = smaller}) ast)
      | smaller <- case hsmodExports of
          Nothing -> [Just $ noLocA []]
          Just exports ->
            [Just (fmap (const entries) exports) | entries <- shrinkList (const []) (unLoc exports)]
      ]
