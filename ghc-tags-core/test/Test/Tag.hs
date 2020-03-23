{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
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
import           System.FilePath (equalFilePath)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Lift as Pipes

import           GhcTags.Tag
import           GhcTags.Stream
import qualified GhcTags.CTag as CTag

import           Test.Tag.Generators


-- TODO add ETags test
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
newtype ArbTag = ArbTag { getArbTag :: CTag }
  deriving Show

genTagAddrLine :: Gen CTag
genTagAddrLine =
          Tag
      <$> (TagName <$> resize 5 genTextNonEmpty)
      <*> genTagKind SingCTag
      <*> genSmallFilePath
      <*> frequency
            [ (8, TagLine . getPositive <$> arbitrary)
            , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> pure NoTagDefinition
      <*> (TagFields <$> listOf genField)

genTagAddrLineCol :: Gen CTag
genTagAddrLineCol =
          Tag
      <$> (TagName <$> resize 5 genTextNonEmpty)
      <*> genTagKind SingCTag
      <*> genSmallFilePath
      <*> frequency
            [ (8, TagLineCol <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary))
            , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
            , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
            ]
      <*> pure NoTagDefinition
      <*> (TagFields <$> listOf genField)

instance Arbitrary ArbTag where
    arbitrary = oneof
      [ ArbTag <$> genTagAddrLine
      , ArbTag <$> genTagAddrLineCol
      ]

    shrink = map ArbTag . shrinkTag . getArbTag


-- | Arbitrary instance with a high probability of gettings the same tags or files.
--
newtype ArbOrdTag = ArbOrdTag { getArbOrdTag :: CTag }
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
             <*> genTagKind SingCTag
             <*> elements
                     [ "Main.hs"
                     , "Lib.hs"
                     ]
             <*> frequency
                   [ (8, TagLine . getPositive <$> arbitrary)
                   , (1, TagCommand . ExCommand . (wrap '/' . fixAddr) <$> genTextNonEmpty)
                   , (1, TagCommand . ExCommand . (wrap '?' . fixAddr) <$> genTextNonEmpty)
                   ]
             <*> pure NoTagDefinition
             <*> pure (TagFields [])

    shrink = map ArbOrdTag . shrinkTag . getArbOrdTag


-- | Generate pairs of tags which are equal in the sense of `compare`.
--
data EqTags = EqTags CTag CTag
  deriving Show

instance Arbitrary EqTags where
    arbitrary = do
      x <- getArbOrdTag <$> arbitrary
      fieldsA <- listOf genField
      fieldsB <- listOf genField
      pure $ EqTags x { tagFields = TagFields fieldsA }
                    x { tagFields = TagFields fieldsB }


-- | Note that this property is weaker than required.  There are unequal `Tag`s
-- in the sense of `==`, which are considered equal by `compare`.
--
ordAntiSymmetryProp :: EqTags -> Bool
ordAntiSymmetryProp (EqTags a b) = a `compareTags` b == EQ


-- We don't provide 'Ord' instance, since it's not compatible with 'compare',
-- see 'weakConsistency'.
--
(≤), (≥) :: Ord (TagAddress tk) => Tag tk -> Tag tk -> Bool
a ≤ b = a `compareTags` b /= GT
a ≥ b = a `compareTags` b /= LT

ordReflexivityyProp :: Ord (TagAddress tk) => Tag tk -> Tag tk -> Bool
ordReflexivityyProp a b = a ≤ b || a ≥ b

ordTransitiveProp :: Ord (TagAddress tk) => Tag tk -> Tag tk -> Tag tk -> Property
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
weakConsistency :: Ord (TagAddress tk) => Tag tk -> Bool
weakConsistency a = a `compareTags` a == EQ


--
-- combineTags properties
--

genSmallFilePath :: Gen String
genSmallFilePath = suchThat (resize 3 arbitrary) (not . null)


-- | sorted list of Tags
newtype ArbTagList = ArbTagList { getArbTagList :: [CTag] }
    deriving Show

instance Arbitrary ArbTagList where
    arbitrary = (ArbTagList . nub . sortBy CTag.compareTags . map getArbTag)
            <$> listOf arbitrary
    shrink (ArbTagList ts) =
      (ArbTagList . sortBy compareTags) `map` shrinkList shrinkTag ts


-- | List of tags from the same file
--
data ArbTagsFromFile = ArbTagsFromFile FilePath [CTag]
    deriving Show

instance Arbitrary ArbTagsFromFile where
    arbitrary = do
      filePath <- genSmallFilePath
      ArbTagList tags <- arbitrary
      let tags' = (\t -> t { tagFilePath = filePath, tagFields = mempty }) `map` tags
      pure $ ArbTagsFromFile filePath (sortBy compareTags tags')

    shrink (ArbTagsFromFile fp tags) =
      [ ArbTagsFromFile fp (sortBy compareTags tags')
      -- Don't shrink file name!
      | tags' <- shrinkList shrinkTag' tags
      ]
      ++
      [ ArbTagsFromFile fp' ((\t -> t { tagFilePath = fp' }) `map` tags)
      | fp' <- shrinkList (const []) fp
      , not (null fp')
      ]



-- properties

combineTags_subset :: ArbTagsFromFile
                   -> [ArbTag]
                   -> Bool
combineTags_subset (ArbTagsFromFile fp as) bs =
    let bs' = getArbTag `map` bs
        cs = combineTags CTag.compareTags fp as bs'
    in all (`elem` cs) as


-- | The tag list be ordered for this property to hold.
--
combineTags_idempotent :: ArbTagsFromFile
                       -> ArbTagList
                       -> Bool
combineTags_idempotent (ArbTagsFromFile fp as) (ArbTagList bs) =
    combineTags CTag.compareTags fp as bs
    == combineTags CTag.compareTags fp as
         (combineTags CTag.compareTags fp as bs)


-- | The tag list cannot connot contain duplicates for this property to hold.
--
combineTags_identity :: ArbTagsFromFile
                     -> Bool
combineTags_identity (ArbTagsFromFile fp as) =
    combineTags CTag.compareTags fp as as == as


-- | Does not modify tags outside of the module.
--
combineTags_preserve :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_preserve (ArbTagsFromFile fp as) (ArbTagList bs) =
       filter (\t -> not $ tagFilePath t `equalFilePath` fp) (combineTags CTag.compareTags fp as bs)
    == 
       filter (\t -> not $ tagFilePath t `equalFilePath` fp) bs


-- | Substitutes all tags of the current file.
--
combineTags_substitution :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_substitution (ArbTagsFromFile fp as) (ArbTagList bs) =
       filter (\t -> tagFilePath t `equalFilePath` fp) (combineTags CTag.compareTags fp as bs)
    == 
       as

-- | 'combineTags' must preserver order of tags.
--
combineTags_order :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_order (ArbTagsFromFile fp as) (ArbTagList bs) =
    let cs = combineTags CTag.compareTags fp as bs
    in sortBy compareTags cs == cs


--
-- combineTagsPipe model test
--

-- | We need a special generator; the property holds only for list of tags
-- which have the same address: `TagLine` or `TagLineCol` but not mixed.
--
-- The reason for that is that the piped `combineTagsPipe` needs to compare
-- tags, and the `Eq` instance cannot distinquishe a tag with address
-- `TagLine 10` with `TagLine 10 3`, even if they are the same tags.  The crux
-- of the problem is that `ctags` have no way of representing a column number.
--
data ArbTagsFromFileAndTagList = ArbTagsFromFileAndTagList FilePath [CTag] [CTag]
  deriving (Eq, Show)

-- | Make addresses monotonic
--
fixAddresses :: [CTag] -> [CTag]
fixAddresses = snd . foldr f (TagLineCol 0 0, [])
  where
    next :: CTagAddress -> CTagAddress
    next (TagLineCol l c) = TagLineCol l (succ c)
    next (TagLine l)      = TagLine (succ l)
    next addr             = addr

    f :: CTag -> (CTagAddress, [CTag]) -> (CTagAddress, [CTag])
    f tag@Tag {tagAddr} (addr, ts) | tagAddr > addr = (tagAddr, tag : ts)
                                   | otherwise      =
                                      let nextAddr = next addr
                                      in (nextAddr, tag { tagAddr = nextAddr } : ts)


instance Arbitrary ArbTagsFromFileAndTagList where
    arbitrary = do
        filePath <- genSmallFilePath
        bool     <- arbitrary
        let tagGen =
              if bool
                then genTagAddrLine
                else genTagAddrLineCol
        tagsFromFile <-
                fixAddresses
              . map (fixFile filePath)
              . nub
              . sortBy compareTags
          <$> listOf tagGen
        tags <- nub
              . sortBy compareTags
          <$> listOf tagGen
        pure $ ArbTagsFromFileAndTagList filePath tagsFromFile tags
      where
        fixFile p t = t { tagFilePath = p
                        , tagFields   = mempty
                        }

    -- A very basic shrinker
    shrink (ArbTagsFromFileAndTagList filePath as bs) =
      [ ArbTagsFromFileAndTagList filePath'
                                  ((\t -> t { tagFilePath = filePath' }) `map` as)
                                  bs
      | filePath' <- shrink filePath 
      , not (null filePath')
      ]
      ++
      [ ArbTagsFromFileAndTagList filePath
                                  ((\t -> t { tagFilePath = filePath }) `map` as')
                                  bs
      | as' <- shrinkList shrinkTag as
      ]
      ++
      [ ArbTagsFromFileAndTagList filePath as bs'
      | bs' <- shrinkList shrinkTag bs
      ]


-- | Check, that the `combineTagsPipe` and agree with it's non-stream version
-- 'combineTags'
--
-- This is an example of a model test (where `combineTags` is regarded a model
-- of `combeinTagsPipe`).
--
combineTagsPipeProp :: ArbTagsFromFileAndTagList -> Property
combineTagsPipeProp (ArbTagsFromFileAndTagList modPath as bs) =
        combineTags CTag.compareTags modPath as bs
    ===
        case
          runStateT
            (Pipes.toListM @(StateT [CTag] Identity)
              (Pipes.for
                 -- yield all `bs`
                (traverse_ Pipes.yield bs)
                (\tag -> Pipes.stateP $ fmap ((),) . combineTagsPipe CTag.compareTags modPath tag)))
            -- take 'as' a state
            as of
        Identity (tags, rest) -> tags ++ rest
