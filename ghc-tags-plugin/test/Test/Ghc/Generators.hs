{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Ghc.Generators (ArbHsDecl (..), ArbClassModule (..), ArbFamilyModule (..), ArbPatternModule (..), ArbMatchModule (..)) where

import Data.Char (isUpper, isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf, nub)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Generics (Generic)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (TypeFamilies, ExplicitNamespaces, BangPatterns, ViewPatterns, NamedFieldPuns, ScopedTypeVariables, NPlusKPatterns, UnboxedTuples, UnboxedSums, LambdaCase, PatternSynonyms, Arrows, TypeApplications, TupleSections, MultiWayIf, TemplateHaskell, TemplateHaskellQuotes, StaticPointers, OverloadedRecordDot, ParallelListComp, TransformListComp, RecursiveDo, ImplicitParams))
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
import GHC.LanguageExtensions.Type (Extension (OrPatterns))
#endif
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
parseModule = parseModuleWith []

-- | Parse a module, enabling additional extensions.
parseModuleWith :: [Extension] -> String -> Located (HsModule GhcPs)
parseModuleWith extensions source =
    case unP Parser.parseModule (initParserState options (stringToStringBuffer source) location) of
      POk _ ast -> ast
      PFailed _ -> error ("Invalid generated module:\n" ++ source)
  where
    location = mkRealSrcLoc (mkFastString "Example.hs") 1 1
    options = mkParserOpts (EnumSet.fromList ([TypeFamilies, ExplicitNamespaces, BangPatterns, ViewPatterns, NamedFieldPuns, ScopedTypeVariables, NPlusKPatterns, UnboxedTuples, UnboxedSums, LambdaCase, PatternSynonyms, Arrows] ++ extensions)) emptyDiagOpts
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


-- Keep the source alongside the native AST so the property can model binder
-- locations independently of the pattern traversal, using identifiable names.
data ArbPatternModule = ArbPatternModule
  { patternModuleSource :: String
  , patternModuleAst :: Located (HsModule GhcPs)
  }

instance Show ArbPatternModule where
    show = patternModuleSource

instance Arbitrary ArbPatternModule where
    arbitrary = do
      ConstructorName suffix <- arbitrary
      pat <- sized (patternBindingSource ("bound" ++ suffix) . min 4)
      let names = nub $ filter ("bound" `isPrefixOf`) $
            words [if isAlphaNum c || c == '_' then c else ' ' | c <- pat]
      exports <- sublistOf names
      exportList <- elements ["", "(" ++ intercalate "," exports ++ ")"]
      signatures <- frequency [(1, pure names), (3, sublistOf names)]
      localSignatures <- sublistOf names
      withLocals <- arbitrary
      let source = unlines $
            ["module Example " ++ exportList ++ " where"]
            ++ [name ++ " :: Int" | name <- signatures]
            ++ ["(" ++ pat ++ ", _) = undefined"]
            ++ if withLocals
                 then ["  where"]
                   ++ ["    " ++ name ++ " :: Int" | name <- localSignatures]
                   ++ ["    (" ++ pat ++ ", _) = undefined"]
                 else []
      pure $ ArbPatternModule source (parseModule source)
    shrink (ArbPatternModule source _) =
      [ ArbPatternModule smaller (parseModule smaller)
      | let sourceLines = lines source
      , smaller <-
          [ unlines (take i sourceLines ++ drop (i + 1) sourceLines)
          | (i, line) <- zip [0 ..] sourceLines
          , "bound" `isPrefixOf` dropWhile isSpace line
          ] ++
          [ unlines ("module Example () where" : drop 1 sourceLines)
          | take 1 sourceLines /= ["module Example () where"]
          ]
      ]


patternBindingSource :: String -> Int -> Gen String
patternBindingSource name depth
  | depth <= 0 = elements [name, "_", "0", "Nothing"]
  | otherwise = oneof
      [ pure name
      , pure "_"
      , pure ("(" ++ name ++ " + 1)")
      , wrap "~(" ")" <$> child "lazy"
      , wrap "!(" ")" <$> child "bang"
      , wrap "(" ")" <$> child "par"
      , wrap "[" "]" <$> child "list"
      , wrap (name ++ "@(") ")" <$> child "as"
      , wrap "(Just (" "))" <$> child "con"
      , wrap "((\\reference -> reference) -> (" "))" <$> child "view"
      , wrap "((" ") :: Maybe reference)" <$> child "sig"
      , wrap "(# " " | #)" <$> child "sum"
      , (\a b -> "(" ++ a ++ ", " ++ b ++ ")") <$> child "left" <*> child "right"
      , (\a b -> "(# " ++ a ++ ", " ++ b ++ " #)") <$> child "left" <*> child "right"
      , (\a b -> "((" ++ a ++ ") : (" ++ b ++ "))") <$> child "head" <*> child "tail"
      , (\a -> "Record { referenceField = (" ++ a ++ "), " ++ name ++ " }") <$> child "field"
      ]
  where
    child suffix = patternBindingSource (name ++ "_" ++ suffix) (depth - 1)
    wrap before after inner = before ++ inner ++ after


-- Only constructors matched by patterns are named "Matched…"; constructors in
-- expressions, types and built-in syntax are not.  Keep the source alongside
-- the native AST so the property can model pattern matches from source tokens.
-- Patterns, expressions, commands and declarations nest each other, so that
-- matches occur in every syntactic context.
data ArbMatchModule = ArbMatchModule
  { matchModuleDeclarations :: [MatchDeclaration]
  , matchModuleSource :: String
  , matchModuleAst :: Located (HsModule GhcPs)
  }

instance Show ArbMatchModule where
    show = matchModuleSource

instance Arbitrary ArbMatchModule where
    arbitrary = sized $ \size -> do
      count <- choose (1, 1 + size `div` 25)
      matchModule <$> vectorOf count arbitrary
    shrink = map matchModule . shrink . matchModuleDeclarations


matchModule :: [MatchDeclaration] -> ArbMatchModule
matchModule declarations =
    ArbMatchModule declarations source (parseModuleWith matchExtensions source)
  where
    source = unlines $
        "module Example where"
      : zipWith matchDeclarationSource [0 ..] declarations


-- | Syntax used by generated match modules.
matchExtensions :: [Extension]
matchExtensions =
    [ TypeApplications, TupleSections, MultiWayIf, TemplateHaskell
    , TemplateHaskellQuotes, StaticPointers, OverloadedRecordDot
    , ParallelListComp, TransformListComp, RecursiveDo, ImplicitParams
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
    , OrPatterns
#endif
    ]


-- | The size of each of the given number of children.
share :: Int -> Int -> Int
share size children = (size - 1) `div` max 1 children

-- | Up to two elements, which share the size.
smallList :: (Int -> Gen a) -> Int -> Gen [a]
smallList gen size
  | size <= 0 = pure []
  | otherwise = do
      count <- choose (0, 2)
      vectorOf count (gen (share size count))


-- | Top-level declarations, which are also generated in declaration quotes.
data MatchDeclaration
  = FunctionDeclaration [PatternSource] RhsSource
  | PatternDeclaration PatternSource RhsSource
  | PatternSynonymDeclaration PatternSource (Maybe ([PatternSource], RhsSource))
  -- ^ a pattern synonym with an optional builder
  | InstanceDeclaration [PatternSource] RhsSource
  | ClassDeclaration [PatternSource] RhsSource
  | SpliceDeclaration ExpressionSource
  | AnnotationDeclaration ExpressionSource
  | RuleDeclaration ExpressionSource ExpressionSource
  deriving (Show, Generic)

instance Arbitrary MatchDeclaration where
    arbitrary = sized genMatchDeclaration
    shrink = genericShrink

genMatchDeclaration :: Int -> Gen MatchDeclaration
genMatchDeclaration size = frequency
    [ (4, uncurry FunctionDeclaration <$> genArguments size)
    , (2, PatternDeclaration <$> genPatternSource half <*> genRhsSource half)
    , (1, PatternSynonymDeclaration <$> genPatternSource half
                                    <*> oneof [pure Nothing, Just <$> genArguments half])
    , (1, uncurry InstanceDeclaration <$> genArguments size)
    , (1, uncurry ClassDeclaration <$> genArguments size)
    , (1, SpliceDeclaration <$> genExpressionSource (size - 1))
    , (1, AnnotationDeclaration <$> genExpressionSource (size - 1))
    , (1, RuleDeclaration <$> genExpressionSource half <*> genExpressionSource half)
    ]
  where
    half = share size 2

-- | Arguments and the right-hand side of a function, which share the size.
genArguments :: Int -> Gen ([PatternSource], RhsSource)
genArguments size = do
    count <- choose (0, 2)
    let n = share size (count + 1)
    (,) <$> vectorOf count (genPatternSource n) <*> genRhsSource n

matchDeclarationSource :: Int -> MatchDeclaration -> String
matchDeclarationSource i declaration =
    case declaration of
      FunctionDeclaration pats rhs ->
        functionName ++ arguments pats ++ rhsSource "=" rhs
      PatternDeclaration pat rhs ->
        "(" ++ patternSource pat ++ ", " ++ functionName ++ ")" ++ rhsSource "=" rhs
      PatternSynonymDeclaration pat builder ->
        "pattern " ++ synonym ++ " <- " ++ patternSource pat
          ++ concat [ " where { " ++ synonym ++ arguments pats ++ rhsSource "=" rhs ++ " }"
                    | Just (pats, rhs) <- [builder]
                    ]
      InstanceDeclaration pats rhs ->
        "instance Class Type" ++ show i ++ " where { method" ++ arguments pats ++ rhsSource "=" rhs ++ " }"
      ClassDeclaration pats rhs ->
        "class Class" ++ show i ++ " a where { method" ++ arguments pats ++ rhsSource "=" rhs ++ " }"
      SpliceDeclaration expr ->
        "$(" ++ expressionSource expr ++ ")"
      AnnotationDeclaration expr ->
        "{-# ANN module " ++ expressionSource expr ++ " #-}"
      RuleDeclaration lhs rhs ->
        "{-# RULES \"rule" ++ show i ++ "\" built " ++ expressionSource lhs
          ++ " = " ++ expressionSource rhs ++ " #-}"
  where
    functionName = "function" ++ show i
    synonym = "Synonym" ++ show i

arguments :: [PatternSource] -> String
arguments = concatMap ((' ' :) . patternSource)


-- | Right-hand sides, with guards and local bindings.
data RhsSource = RhsSource [GuardSource] ExpressionSource LocalBindsSource
  deriving (Show, Generic)

instance Arbitrary RhsSource where
    arbitrary = sized genRhsSource
    shrink = genericShrink

genRhsSource :: Int -> Gen RhsSource
genRhsSource size = do
    guards <- frequency [(2, pure 0), (1, choose (1, 2))]
    local <- arbitrary
    let n = share size (1 + guards + fromEnum local)
    RhsSource <$> vectorOf guards (genGuardSource n)
              <*> genExpressionSource n
              <*> if local then genLocalBindsSource n else pure (ValueBindings [])

rhsSource :: String -> RhsSource -> String
rhsSource arrow (RhsSource guards body locals) =
       concat [ " | " ++ intercalate ", " (map guardSource guards) | not (null guards) ]
    ++ " " ++ arrow ++ " " ++ expressionSource body
    ++ case locals of
         ValueBindings [] -> ""
         _                -> " where " ++ localBindsSource locals


data GuardSource
  = BooleanGuard ExpressionSource
  | PatternGuard PatternSource ExpressionSource
  | LetGuard LocalBindsSource
  deriving (Show, Generic)

instance Arbitrary GuardSource where
    arbitrary = sized genGuardSource
    shrink = genericShrink

genGuardSource :: Int -> Gen GuardSource
genGuardSource size
  | size <= 0 = pure (BooleanGuard Built)
  | otherwise = oneof
      [ BooleanGuard <$> genExpressionSource (size - 1)
      , PatternGuard <$> genPatternSource half <*> genExpressionSource half
      , LetGuard <$> genLocalBindsSource (size - 1)
      ]
  where
    half = share size 2

guardSource :: GuardSource -> String
guardSource guardSyntax = case guardSyntax of
    BooleanGuard expr -> expressionSource expr
    PatternGuard pat expr -> patternSource pat ++ " <- " ++ expressionSource expr
    LetGuard locals -> letSource locals


-- | Local bindings, in @let@ and @where@: value bindings, or an implicit
-- parameter binding, which cannot be mixed with value bindings.
data LocalBindsSource
  = ValueBindings [BindingSource]
  | ImplicitParameterBinding ExpressionSource
  deriving (Show, Generic)

instance Arbitrary LocalBindsSource where
    arbitrary = sized genLocalBindsSource
    shrink = genericShrink

genLocalBindsSource :: Int -> Gen LocalBindsSource
genLocalBindsSource size = frequency
    [ (4, ValueBindings <$> smallList genBindingSource size)
    , (1, ImplicitParameterBinding <$> genExpressionSource (size - 1))
    ]

localBindsSource :: LocalBindsSource -> String
localBindsSource locals = case locals of
    ValueBindings bindings -> "{ " ++ intercalate "; " (map bindingSource bindings) ++ " }"
    ImplicitParameterBinding expr -> "{ ?parameter = " ++ expressionSource expr ++ " }"

letSource :: LocalBindsSource -> String
letSource locals = "let " ++ localBindsSource locals


data BindingSource
  = FunctionBinding [PatternSource] RhsSource
  | PatternBinding PatternSource RhsSource
  deriving (Show, Generic)

instance Arbitrary BindingSource where
    arbitrary = sized genBindingSource
    shrink = genericShrink

genBindingSource :: Int -> Gen BindingSource
genBindingSource size = oneof
    [ uncurry FunctionBinding <$> genArguments size
    , PatternBinding <$> genPatternSource half <*> genRhsSource half
    ]
  where
    half = share size 2

bindingSource :: BindingSource -> String
bindingSource binding = case binding of
    FunctionBinding pats rhs -> "local" ++ arguments pats ++ rhsSource "=" rhs
    PatternBinding pat rhs -> "(" ++ patternSource pat ++ ", local)" ++ rhsSource "=" rhs


-- | @case@ alternatives.
data AlternativeSource = AlternativeSource PatternSource RhsSource
  deriving (Show, Generic)

instance Arbitrary AlternativeSource where
    arbitrary = sized genAlternativeSource
    shrink = genericShrink

genAlternativeSource :: Int -> Gen AlternativeSource
genAlternativeSource size =
    AlternativeSource <$> genPatternSource half <*> genRhsSource half
  where
    half = share size 2

alternativesSource :: [AlternativeSource] -> String
alternativesSource alternatives =
    "{ " ++ intercalate "; " [ patternSource pat ++ rhsSource "->" rhs | AlternativeSource pat rhs <- alternatives ] ++ " }"


-- | Statements of @do@ blocks.
data StatementSource
  = BindStatement PatternSource ExpressionSource
  | BodyStatement ExpressionSource
  | LetStatement LocalBindsSource
  | RecStatement [StatementSource]
  deriving (Show, Generic)

instance Arbitrary StatementSource where
    arbitrary = sized genStatementSource
    shrink = genericShrink

genStatementSource :: Int -> Gen StatementSource
genStatementSource size
  | size <= 0 = pure (BodyStatement Built)
  | otherwise = oneof
      [ BindStatement <$> genPatternSource half <*> genExpressionSource half
      , BodyStatement <$> genExpressionSource (size - 1)
      , LetStatement <$> genLocalBindsSource (size - 1)
      , RecStatement <$> smallList genStatementSource size
      ]
  where
    half = share size 2

statementSource :: StatementSource -> String
statementSource statement = case statement of
    BindStatement pat expr -> patternSource pat ++ " <- " ++ expressionSource expr
    BodyStatement expr -> expressionSource expr
    LetStatement locals -> letSource locals
    RecStatement statements -> "rec { " ++ intercalate "; " (map statementSource statements) ++ " }"


-- | A branch of a list comprehension, which has at least one qualifier.
data QualifiersSource = QualifiersSource QualifierSource [QualifierSource]
  deriving (Show, Generic)

instance Arbitrary QualifiersSource where
    arbitrary = sized genQualifiersSource
    shrink = genericShrink

genQualifiersSource :: Int -> Gen QualifiersSource
genQualifiersSource size = do
    count <- choose (0, 2)
    let n = share size (count + 1)
    QualifiersSource <$> genQualifierSource n <*> vectorOf count (genQualifierSource n)

qualifiersSource :: QualifiersSource -> String
qualifiersSource (QualifiersSource qualifier qualifiers) =
    intercalate ", " (map qualifierSource (qualifier : qualifiers))


data QualifierSource
  = BindQualifier PatternSource ExpressionSource
  | GuardQualifier ExpressionSource
  | LetQualifier LocalBindsSource
  | TransformQualifier (Maybe ExpressionSource)
  -- ^ @then@, optionally with @by@
  deriving (Show, Generic)

instance Arbitrary QualifierSource where
    arbitrary = sized genQualifierSource
    shrink = genericShrink

genQualifierSource :: Int -> Gen QualifierSource
genQualifierSource size
  | size <= 0 = pure (GuardQualifier Built)
  | otherwise = oneof
      [ BindQualifier <$> genPatternSource half <*> genExpressionSource half
      , GuardQualifier <$> genExpressionSource (size - 1)
      , LetQualifier <$> genLocalBindsSource (size - 1)
      , TransformQualifier <$> frequency [(1, pure Nothing), (2, Just <$> genExpressionSource (size - 1))]
      ]
  where
    half = share size 2

qualifierSource :: QualifierSource -> String
qualifierSource qualifier = case qualifier of
    BindQualifier pat expr -> patternSource pat ++ " <- " ++ expressionSource expr
    GuardQualifier expr -> expressionSource expr
    LetQualifier locals -> letSource locals
    TransformQualifier Nothing -> "then built"
    TransformQualifier (Just expr) -> "then built by " ++ expressionSource expr


-- | Arrow commands.
data CommandSource
  = ArrowApplication ExpressionSource ExpressionSource
  | ArrowForm ExpressionSource [CommandSource]
  | CommandApplication CommandSource ExpressionSource
  | CommandLambda PatternSource CommandSource
  | CommandLambdaCase CommandAlternative [CommandAlternative]
  | CommandCase ExpressionSource CommandAlternative [CommandAlternative]
  | CommandIf ExpressionSource CommandSource CommandSource
  | CommandLet LocalBindsSource CommandSource
  | CommandDo [CommandStatement] CommandSource
  deriving (Show, Generic)

instance Arbitrary CommandSource where
    arbitrary = sized genCommandSource
    shrink = genericShrink

genCommandSource :: Int -> Gen CommandSource
genCommandSource size
  | size <= 0 = pure (ArrowApplication Built Built)
  | otherwise = frequency
      [ (2, ArrowApplication <$> genExpressionSource half <*> genExpressionSource half)
      , (2, do count <- choose (1, 2)
               let n = share size (count + 1)
               ArrowForm <$> genExpressionSource n <*> vectorOf count (genCommandSource n))
      , (2, CommandApplication <$> genCommandSource half <*> genExpressionSource half)
      , (1, CommandLambda <$> genPatternSource half <*> genCommandSource half)
      , (1, CommandLambdaCase <$> genCommandAlternative half <*> smallList genCommandAlternative half)
      , (1, CommandCase <$> genExpressionSource third <*> genCommandAlternative third
                        <*> smallList genCommandAlternative third)
      , (1, CommandIf <$> genExpressionSource third <*> genCommandSource third <*> genCommandSource third)
      , (1, CommandLet <$> genLocalBindsSource half <*> genCommandSource half)
      , (1, CommandDo <$> smallList genCommandStatement half <*> genCommandSource half)
      ]
  where
    half = share size 2
    third = share size 3

commandSource :: CommandSource -> String
commandSource command = case command of
    ArrowApplication arrow input ->
      "(" ++ expressionSource arrow ++ " -< " ++ expressionSource input ++ ")"
    ArrowForm operator commands ->
      "(| " ++ expressionSource operator ++ concatMap ((' ' :) . commandSource) commands ++ " |)"
    CommandApplication cmd expr ->
      "(" ++ commandSource cmd ++ " " ++ expressionSource expr ++ ")"
    CommandLambda pat cmd ->
      "(\\ " ++ patternSource pat ++ " -> " ++ commandSource cmd ++ ")"
    CommandLambdaCase alternative alternatives ->
      "(\\case " ++ commandAlternativesSource (alternative : alternatives) ++ ")"
    CommandCase expr alternative alternatives ->
      "(case " ++ expressionSource expr ++ " of " ++ commandAlternativesSource (alternative : alternatives) ++ ")"
    CommandIf condition consequent alternative ->
      "(if " ++ expressionSource condition ++ " then " ++ commandSource consequent
        ++ " else " ++ commandSource alternative ++ ")"
    CommandLet locals cmd ->
      "(" ++ letSource locals ++ " in " ++ commandSource cmd ++ ")"
    CommandDo statements cmd ->
      "(do { " ++ concatMap ((++ "; ") . commandStatementSource) statements ++ commandSource cmd ++ " })"


data CommandAlternative = CommandAlternative PatternSource [GuardSource] CommandSource
  deriving (Show, Generic)

instance Arbitrary CommandAlternative where
    arbitrary = sized genCommandAlternative
    shrink = genericShrink

genCommandAlternative :: Int -> Gen CommandAlternative
genCommandAlternative size = do
    guards <- frequency [(2, pure 0), (1, choose (1, 2))]
    let n = share size (guards + 2)
    CommandAlternative <$> genPatternSource n <*> vectorOf guards (genGuardSource n) <*> genCommandSource n

commandAlternativesSource :: [CommandAlternative] -> String
commandAlternativesSource alternatives =
    "{ " ++ intercalate "; "
      [ patternSource pat
          ++ concat [ " | " ++ intercalate ", " (map guardSource guards) | not (null guards) ]
          ++ " -> " ++ commandSource cmd
      | CommandAlternative pat guards cmd <- alternatives
      ]
    ++ " }"


data CommandStatement
  = CommandBind PatternSource CommandSource
  | CommandBody CommandSource
  | CommandLetStatement LocalBindsSource
  | CommandRec [CommandStatement]
  deriving (Show, Generic)

instance Arbitrary CommandStatement where
    arbitrary = sized genCommandStatement
    shrink = genericShrink

genCommandStatement :: Int -> Gen CommandStatement
genCommandStatement size
  | size <= 0 = pure (CommandBody (ArrowApplication Built Built))
  | otherwise = oneof
      [ CommandBind <$> genPatternSource half <*> genCommandSource half
      , CommandBody <$> genCommandSource (size - 1)
      , CommandLetStatement <$> genLocalBindsSource (size - 1)
      , CommandRec <$> smallList genCommandStatement size
      ]
  where
    half = share size 2

commandStatementSource :: CommandStatement -> String
commandStatementSource statement = case statement of
    CommandBind pat cmd -> patternSource pat ++ " <- " ++ commandSource cmd
    CommandBody cmd -> commandSource cmd
    CommandLetStatement locals -> letSource locals
    CommandRec statements -> "rec { " ++ intercalate "; " (map commandStatementSource statements) ++ " }"


-- | Expressions, which render to atomic expressions, possibly spanning lines.
data ExpressionSource
  = Built
  | Application ExpressionSource ExpressionSource
  | TypeApplication ExpressionSource
  | Operator ExpressionSource ExpressionSource
  | Negation ExpressionSource
  | LeftSection ExpressionSource
  | RightSection ExpressionSource
  | TupleExpression ExpressionSource ExpressionSource
  | TupleSection ExpressionSource
  | SumExpression ExpressionSource
  | ListExpression [ExpressionSource]
  | Sequence ExpressionSource (Maybe ExpressionSource) (Maybe ExpressionSource)
  -- ^ an arithmetic sequence: from, then and to
  | If ExpressionSource ExpressionSource ExpressionSource
  | MultiIf GuardSource ExpressionSource
  | Case ExpressionSource AlternativeSource [AlternativeSource]
  | Lambda PatternSource ExpressionSource
  | LambdaCaseExpression AlternativeSource [AlternativeSource]
  | Let LocalBindsSource ExpressionSource
  | Do [StatementSource] ExpressionSource
  | Comprehension ExpressionSource QualifiersSource [QualifiersSource]
  -- ^ a list comprehension with parallel branches
  | RecordConstruction ExpressionSource
  | RecordUpdate ExpressionSource ExpressionSource
  | FieldSelection ExpressionSource
  | TypeSignature ExpressionSource
  | Static ExpressionSource
  | Pragma ExpressionSource
  | Splice ExpressionSource
  | TypedSplice ExpressionSource
  | ExpressionQuote ExpressionSource
  | TypedQuote ExpressionSource
  | PatternQuote PatternSource
  | DeclarationQuote [MatchDeclaration]
  | Proc PatternSource CommandSource
  | ExpressionNewLine ExpressionSource
  deriving (Show, Generic)

instance Arbitrary ExpressionSource where
    arbitrary = sized genExpressionSource
    shrink = genericShrink

genExpressionSource :: Int -> Gen ExpressionSource
genExpressionSource size
  | size <= 0 = pure Built
  | otherwise = frequency
      [ (3, pure Built)
      , (1, Application <$> expression half <*> expression half)
      , (1, TypeApplication <$> expression smaller)
      , (1, Operator <$> expression half <*> expression half)
      , (1, Negation <$> expression smaller)
      , (1, LeftSection <$> expression smaller)
      , (1, RightSection <$> expression smaller)
      , (1, TupleExpression <$> expression half <*> expression half)
      , (1, TupleSection <$> expression smaller)
      , (1, SumExpression <$> expression smaller)
      , (1, ListExpression <$> smallList genExpressionSource size)
      , (1, Sequence <$> expression third <*> optional third <*> optional third)
      , (1, If <$> expression third <*> expression third <*> expression third)
      , (1, MultiIf <$> genGuardSource half <*> expression half)
      , (3, Case <$> expression third <*> genAlternativeSource third <*> smallList genAlternativeSource third)
      , (3, Lambda <$> genPatternSource half <*> expression half)
      , (2, LambdaCaseExpression <$> genAlternativeSource half <*> smallList genAlternativeSource half)
      , (2, Let <$> genLocalBindsSource half <*> expression half)
      , (2, Do <$> smallList genStatementSource half <*> expression half)
      , (2, Comprehension <$> expression third <*> genQualifiersSource third <*> smallList genQualifiersSource third)
      , (1, RecordConstruction <$> expression smaller)
      , (1, RecordUpdate <$> expression half <*> expression half)
      , (1, FieldSelection <$> expression smaller)
      , (1, TypeSignature <$> expression smaller)
      , (1, Static <$> expression smaller)
      , (1, Pragma <$> expression smaller)
      , (1, Splice <$> expression smaller)
      , (1, TypedSplice <$> expression smaller)
      , (1, ExpressionQuote <$> expression smaller)
      , (1, TypedQuote <$> expression smaller)
      , (1, PatternQuote <$> genPatternSource smaller)
      , (1, DeclarationQuote <$> smallList genMatchDeclaration size)
      , (3, Proc <$> genPatternSource half <*> genCommandSource half)
      , (1, ExpressionNewLine <$> expression smaller)
      ]
  where
    expression = genExpressionSource
    optional n = oneof [pure Nothing, Just <$> expression n]
    smaller = size - 1
    half = share size 2
    third = share size 3

expressionSource :: ExpressionSource -> String
expressionSource expr = case expr of
    Built -> "Built"
    Application f e -> "(" ++ expressionSource f ++ " " ++ expressionSource e ++ ")"
    TypeApplication e -> "(" ++ expressionSource e ++ " @Type)"
    Operator l r -> "(" ++ expressionSource l ++ " + " ++ expressionSource r ++ ")"
    Negation e -> "(- " ++ expressionSource e ++ ")"
    LeftSection e -> "(" ++ expressionSource e ++ " +)"
    RightSection e -> "(+ " ++ expressionSource e ++ ")"
    TupleExpression a b -> "(" ++ expressionSource a ++ ", " ++ expressionSource b ++ ")"
    TupleSection e -> "(" ++ expressionSource e ++ ", )"
    SumExpression e -> "(# " ++ expressionSource e ++ " | #)"
    ListExpression es -> "[" ++ intercalate ", " (map expressionSource es) ++ "]"
    Sequence from next to ->
      "[" ++ expressionSource from ++ concat [ ", " ++ expressionSource e | Just e <- [next] ]
        ++ " .." ++ concat [ ' ' : expressionSource e | Just e <- [to] ] ++ "]"
    If c t e -> "(if " ++ expressionSource c ++ " then " ++ expressionSource t ++ " else " ++ expressionSource e ++ ")"
    MultiIf g e -> "(if { | " ++ guardSource g ++ " -> " ++ expressionSource e ++ " })"
    Case e alternative alternatives ->
      "(case " ++ expressionSource e ++ " of " ++ alternativesSource (alternative : alternatives) ++ ")"
    Lambda p e -> "(\\ " ++ patternSource p ++ " -> " ++ expressionSource e ++ ")"
    LambdaCaseExpression alternative alternatives -> "(\\case " ++ alternativesSource (alternative : alternatives) ++ ")"
    Let locals e -> "(" ++ letSource locals ++ " in " ++ expressionSource e ++ ")"
    Do statements e -> "(do { " ++ concatMap ((++ "; ") . statementSource) statements ++ expressionSource e ++ " })"
    Comprehension e branch branches ->
      "[ " ++ expressionSource e ++ concatMap ((" | " ++) . qualifiersSource) (branch : branches) ++ " ]"
    RecordConstruction e -> "(Built { field = " ++ expressionSource e ++ " })"
    RecordUpdate r e -> "((" ++ expressionSource r ++ ") { field = " ++ expressionSource e ++ " })"
    FieldSelection e -> "((" ++ expressionSource e ++ ").field)"
    TypeSignature e -> "(" ++ expressionSource e ++ " :: Type)"
    Static e -> "(static " ++ expressionSource e ++ ")"
    Pragma e -> "({-# SCC \"cost\" #-} " ++ expressionSource e ++ ")"
    Splice e -> "$(" ++ expressionSource e ++ ")"
    TypedSplice e -> "$$(" ++ expressionSource e ++ ")"
    ExpressionQuote e -> "[| " ++ expressionSource e ++ " |]"
    TypedQuote e -> "[|| " ++ expressionSource e ++ " ||]"
    PatternQuote p -> "[p| " ++ patternSource p ++ " |]"
    DeclarationQuote declarations ->
      "[d| { " ++ intercalate "; " (zipWith matchDeclarationSource [0 ..] declarations) ++ " } |]"
    Proc p cmd -> "(proc " ++ patternSource p ++ " -> " ++ commandSource cmd ++ ")"
    ExpressionNewLine e -> "\n    " ++ expressionSource e


-- | Patterns, which render to atomic patterns, possibly spanning lines.
data PatternSource
  = Variable
  | Wildcard
  | Literal
  | NegativeLiteral
  | Successor
  -- ^ an n+k pattern
  | Nil
  | Unit
  | Constructor ConstructorName [PatternSource]
  | Infix ConstructorName PatternSource PatternSource
  | Record ConstructorName [PatternSource]
  | Cons PatternSource PatternSource
  | Tuple PatternSource PatternSource
  | UnboxedTuple PatternSource PatternSource
  | UnboxedSum PatternSource
  | List [PatternSource]
  | As PatternSource
  | Lazy PatternSource
  | Bang PatternSource
  | View ExpressionSource PatternSource
  -- ^ a view function, and the matched pattern
  | Signature PatternSource
  | PatternSplice ExpressionSource
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
  | Or PatternSource PatternSource
#endif
  | NewLine PatternSource
  deriving (Show, Generic)

instance Arbitrary PatternSource where
    arbitrary = sized genPatternSource
    shrink pat = case pat of
      Constructor _ pats -> pats ++ genericShrink pat
      Record _ pats      -> pats ++ genericShrink pat
      List pats          -> pats ++ genericShrink pat
      _                  -> genericShrink pat


genPatternSource :: Int -> Gen PatternSource
genPatternSource size
  | size <= 0 = leaf
  | otherwise = frequency
      [ (2, leaf)
      , (4, Constructor <$> arbitrary <*> smallList genPatternSource size)
      , (2, Infix <$> arbitrary <*> pattern half <*> pattern half)
      , (2, Record <$> arbitrary <*> smallList genPatternSource size)
      , (1, Cons <$> pattern half <*> pattern half)
      , (1, Tuple <$> pattern half <*> pattern half)
      , (1, UnboxedTuple <$> pattern half <*> pattern half)
      , (1, UnboxedSum <$> pattern smaller)
      , (1, List <$> smallList genPatternSource size)
      , (1, As <$> pattern smaller)
      , (1, Lazy <$> pattern smaller)
      , (1, Bang <$> pattern smaller)
      , (1, View <$> genExpressionSource half <*> pattern half)
      , (1, Signature <$> pattern smaller)
      , (1, PatternSplice <$> genExpressionSource smaller)
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
      , (1, Or <$> pattern half <*> pattern half)
#endif
      , (1, NewLine <$> pattern smaller)
      ]
  where
    leaf = elements [Variable, Wildcard, Literal, NegativeLiteral, Successor, Nil, Unit]
    pattern = genPatternSource
    smaller = size - 1
    half = share size 2


patternSource :: PatternSource -> String
patternSource pat = case pat of
    Variable -> "x"
    Wildcard -> "_"
    Literal -> "0"
    NegativeLiteral -> "(-1)"
    Successor -> "(n + 1)"
    Nil -> "[]"
    Unit -> "()"
    Constructor name pats -> "(" ++ unwords (matched name : map patternSource pats) ++ ")"
    Infix name a b -> "(" ++ patternSource a ++ " `" ++ matched name ++ "` " ++ patternSource b ++ ")"
    Record name pats ->
      matched name ++ " { "
        ++ intercalate ", " ["field" ++ show i ++ " = " ++ patternSource p | (i, p) <- zip [0 :: Int ..] pats]
        ++ " }"
    Cons a b -> "(" ++ patternSource a ++ " : " ++ patternSource b ++ ")"
    Tuple a b -> "(" ++ patternSource a ++ ", " ++ patternSource b ++ ")"
    UnboxedTuple a b -> "(# " ++ patternSource a ++ ", " ++ patternSource b ++ " #)"
    UnboxedSum p -> "(# " ++ patternSource p ++ " | #)"
    List pats -> "[" ++ intercalate ", " (map patternSource pats) ++ "]"
    As p -> "x@(" ++ patternSource p ++ ")"
    Lazy p -> "(~(" ++ patternSource p ++ "))"
    Bang p -> "(!(" ++ patternSource p ++ "))"
    View viewExpr p -> "(" ++ expressionSource viewExpr ++ " -> " ++ patternSource p ++ ")"
    Signature p -> "(" ++ patternSource p ++ " :: Type)"
    PatternSplice e -> "$(" ++ expressionSource e ++ ")"
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
    Or a b -> "(" ++ patternSource a ++ "; " ++ patternSource b ++ ")"
#endif
    NewLine p -> "\n    " ++ patternSource p
  where
    matched (ConstructorName name) = "Matched" ++ name
