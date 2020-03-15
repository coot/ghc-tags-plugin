{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Test.Tag (tests) where

import           Control.Monad.State.Strict
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Foldable (traverse_)
import           Data.List (nub, sortBy)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Lift as Pipes

import           Plugin.GhcTags.Tag
import           Plugin.GhcTags.Stream

import           Test.Tag.Generators


tests :: TestTree
tests = testGroup "Tag"
  [ testGroup "compareTags"
    [ testProperty "antisymmetry" ordAntiSymmetryProp
    , testProperty "reflexivity"  (on ordReflexivityyProp getArbOrdTag)
    , testProperty "transitivity" (\a b c ->
                                          ordTransitiveProp
                                            (getArbOrdTag a)
                                            (getArbOrdTag b)
                                            (getArbOrdTag c))
    , testProperty "Eq:consistency"  (weakConsistency . getArbOrdTag)
    , testProperty "sort:idempotent" sortIdempotentProp
    ]
  , testGroup "combineTags"
    [ testProperty "subset"       combineTags_subset
    , testProperty "idempotent"   combineTags_idempotent
    , testProperty "identity"     combineTags_identity
    , testProperty "preserve"     combineTags_preserve
    , testProperty "substitution" combineTags_substitution
    , testProperty "order"        combineTags_order
    ]
  , testGroup "combineTagsPipe"
    [ testProperty "model test"   combineTagsPipeProp
    ]
  ]


-- | 'Tag' generator
--
newtype ArbTag = ArbTag { getArbTag :: Tag }
  deriving Show

genTagAddrLine :: Gen Tag
genTagAddrLine =
          Tag
      <$> (TagName <$> resize 5 genTextNonEmpty)
      <*> genTagKind
      <*> (TagFile <$> genSmallFilePath)
      <*> frequency
            [ (8, TagLine . getPositive <$> arbitrary)
            , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> listOf genField

genTagAddrLineCol :: Gen Tag
genTagAddrLineCol =
          Tag
      <$> (TagName <$> resize 5 genTextNonEmpty)
      <*> genTagKind
      <*> (TagFile <$> genSmallFilePath)
      <*> frequency
            [ (8, TagLineCol <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary))
            , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> listOf genField

instance Arbitrary ArbTag where
    arbitrary = oneof
      [ ArbTag <$> genTagAddrLine
      , ArbTag <$> genTagAddrLineCol
      ]

    shrink = map ArbTag . shrinkTag . getArbTag


-- | Arbitrary instance with a high probability of gettings the same tags or files.
--
newtype ArbOrdTag = ArbOrdTag { getArbOrdTag :: Tag }
  deriving Show


instance Arbitrary ArbOrdTag where
    arbitrary = fmap ArbOrdTag
              $  Tag
             <$> elements
                   (TagName `map`
                     [ "find"
                     , "Ord"
                     , "Eq"
                     ])
             <*> genTagKind
             <*> elements
                   (TagFile `map`
                     [ "Main.hs"
                     , "Lib.hs"
                     ])
             <*> frequency
                   [ (8, TagLine . getPositive <$> arbitrary)
                   , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
                   , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
                   ]
             <*> pure []

    shrink = map ArbOrdTag . shrinkTag . getArbOrdTag


-- | Generate pairs of tags which are equal in the sense of `compare`.
--
data EqTags = EqTags Tag Tag
  deriving Show

instance Arbitrary EqTags where
    arbitrary = do
      x <- getArbOrdTag <$> arbitrary
      fieldsA <- listOf genField
      fieldsB <- listOf genField
      pure $ EqTags x { tagFields = fieldsA }
                    x { tagFields = fieldsB }


-- | Note that this property is weaker than required.  There are unequal `Tag`s
-- in the sense of `==`, which are considered equal by `compare`.
--
ordAntiSymmetryProp :: EqTags -> Bool
ordAntiSymmetryProp (EqTags a b) = a `compareTags` b == EQ


-- We don't provide 'Ord' instance, since it's not compatible with 'compare',
-- see 'weakConsistency'.
--
(≤), (≥) :: Tag -> Tag -> Bool
a ≤ b = a `compareTags` b /= GT
a ≥ b = a `compareTags` b /= LT

ordReflexivityyProp :: Tag -> Tag -> Bool
ordReflexivityyProp a b = a ≤ b || a ≥ b

ordTransitiveProp :: Tag -> Tag -> Tag -> Property
ordTransitiveProp a b c =
       a ≤ b && b ≤ c
    || a ≥ b && b ≥ c ==>
    if | a ≤ b && b ≤ c -> a ≤ c
       | a ≥ b && b ≥ c -> a ≥ c
       | otherwise      -> error "impossible happened"
  where

sortIdempotentProp :: [ArbTag] -> Bool
sortIdempotentProp ts =
    let ts' = getArbTag `map` ts
        ts'' = sortBy compareTags ts'
    in sortBy compareTags ts'' == ts''


-- | The
--
-- prop> a == b ==> a `compare` b == EQ`
--
-- But since 'Tag' is using derived 'Eq' instance, it is equivalent to
weakConsistency :: Tag -> Bool
weakConsistency a = a `compareTags` a == EQ


--
-- combineTags properties
--

genSmallFilePath :: Gen String
genSmallFilePath = suchThat (resize 3 arbitrary) (not . null)


-- | sorted list of Tags
newtype ArbTagList = ArbTagList { getArbTagList :: [Tag] }
    deriving Show

instance Arbitrary ArbTagList where
    arbitrary = (ArbTagList . nub . sortBy compareTags . map getArbTag)
            <$> listOf arbitrary
    shrink (ArbTagList ts) =
      (ArbTagList . sortBy compareTags) `map` shrinkList shrinkTag ts


-- | List of tags from the same file
--
data ArbTagsFromFile = ArbTagsFromFile FilePath [Tag]
    deriving Show

instance Arbitrary ArbTagsFromFile where
    arbitrary = do
      filePath <- genSmallFilePath
      ArbTagList tags <- arbitrary
      let tags' = (\t -> t { tagFile = TagFile filePath, tagFields = [] }) `map` tags
      pure $ ArbTagsFromFile filePath (sortBy compareTags tags')

    shrink (ArbTagsFromFile fp tags) =
      [ ArbTagsFromFile fp (sortBy compareTags tags')
      -- Don't shrink file name!
      | tags' <- shrinkList shrinkTag' tags
      ]
      ++
      [ ArbTagsFromFile fp' ((\t -> t { tagFile = TagFile fp' }) `map` tags)
      | fp' <- shrinkList (const []) fp
      , not (null fp')
      ]



-- properties

combineTags_subset :: ArbTagsFromFile
                   -> [ArbTag]
                   -> Bool
combineTags_subset (ArbTagsFromFile _ as) bs =
    let bs' = getArbTag `map` bs
        cs = as `combineTags` bs'
    in all (`elem` cs) as


-- | The tag list be ordered for this property to hold.
--
combineTags_idempotent :: ArbTagList
                       -> ArbTagList
                       -> Bool
combineTags_idempotent (ArbTagList as) (ArbTagList bs) =
    combineTags as bs == combineTags as (combineTags as bs)


-- | The tag list cannot connot contain duplicates for this property to hold.
--
combineTags_identity :: ArbTagList
                     -> Bool
combineTags_identity (ArbTagList as) =
    combineTags as as == as


-- | Does not modify tags outside of the module.
--
combineTags_preserve :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_preserve (ArbTagsFromFile fp as) (ArbTagList bs) =
       filter (\t -> tagFilePath t /= fp) (as `combineTags` bs)
    == 
       filter (\t -> tagFilePath t /= fp) bs


-- | Substitutes all tags of the current file.
--
combineTags_substitution :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_substitution (ArbTagsFromFile fp as) (ArbTagList bs) =
       filter (\t -> tagFilePath t == fp) (as `combineTags` bs)
    == 
       as

-- | 'combineTags' must preserver order of tags.
--
combineTags_order :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_order (ArbTagsFromFile _ as) (ArbTagList bs) =
    let cs = as `combineTags` bs
    in sortBy compareTags cs == cs


--
-- combineTagsPipe model test
--

-- | We need a special generator; the property holds only for list of tags
-- which have the same address: `TagLine` or `TagLineCol` but not mixed.
--
-- The reason for that is that the piped `combineTagsPipe` needs to compare
-- tags, and the `Eq` instance cannot distinquishes a tag with address `TagLine
-- 10` with `TagLine 10 3`, even if they are the same tags.  The problem is
-- that `ctags` have no way to represent column number.
--
data ArbTagsFromFileAndTagList = ArbTagsFromFileAndTagList [Tag] [Tag]
  deriving (Eq, Show)

instance Arbitrary ArbTagsFromFileAndTagList where
    arbitrary = do
        filePath <- genSmallFilePath
        bool     <- arbitrary
        let tagGen = if bool then genTagAddrLine else genTagAddrLineCol
        tagsFromFile <- map (fixFile filePath) . nub . sortBy compareTags <$> listOf tagGen
        tags <- nub . sortBy compareTags <$> listOf tagGen
        pure $ ArbTagsFromFileAndTagList tagsFromFile tags
      where
        fixFile p t = t { tagFile = TagFile p, tagFields = [] }

    -- TODO shrink



-- | Check, that the `combineTagsPipe` and agree with it's non-stream version
-- 'combineTags'
--
-- This is an example of a model test (where `combineTags` is regarded a model
-- of `combeinTagsPipe`).
--
combineTagsPipeProp :: ArbTagsFromFile -> ArbTagList -> Property
combineTagsPipeProp (ArbTagsFromFile _ as) (ArbTagList bs) =
        as `combineTags` bs
    ===
        case
          runStateT
            (Pipes.toListM @(StateT [Tag] Identity)
              (Pipes.for
                 -- yield all `bs`
                (traverse_ Pipes.yield bs)
                (\tag -> Pipes.stateP $ fmap ((),) . combineTagsPipe tag)))
            -- take 'as' a state
            as of
        Identity (tags, rest) -> tags ++ rest


