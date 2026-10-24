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

import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Foldable (traverse_)
import           Data.List (nub, sortBy)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import qualified Pipes
import qualified Pipes.Prelude as Pipes

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
  , testGroup "combineTagsStream"
    [ testProperty "model test"   combineTagsStreamProp
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
                     [ TagFilePath "Main.hs"
                     , TagFilePath "Lib.hs"
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
(≤), (≥) :: Tag tk -> Tag tk -> Bool
a ≤ b = a `compareTags` b /= GT
a ≥ b = a `compareTags` b /= LT

ordReflexivityyProp :: Tag tk -> Tag tk -> Bool
ordReflexivityyProp a b = a ≤ b || a ≥ b

ordTransitiveProp :: Tag tk -> Tag tk -> Tag tk -> Property
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
        ts'' = sortBy CTag.compareTags ts'
    in sortBy CTag.compareTags ts'' == ts''


-- | The
--
-- prop> a == b ==> a `compare` b == EQ`
--
-- But since 'Tag' is using derived 'Eq' instance, it is equivalent to
weakConsistency :: Tag tk -> Bool
weakConsistency a = a `compareTags` a == EQ


--
-- combineTags properties
--

genSmallFilePath :: Gen TagFilePath
genSmallFilePath = TagFilePath <$> suchThat (resize 3 arbitrary) (not . Text.null)


-- | sorted list of Tags
newtype ArbTagList = ArbTagList { getArbTagList :: [CTag] }
    deriving Show

instance Arbitrary ArbTagList where
    arbitrary = (ArbTagList . nub . sortBy CTag.compareTags . map getArbTag)
            <$> listOf arbitrary
    shrink (ArbTagList ts) =
      (ArbTagList . sortBy CTag.compareTags) `map` shrinkList shrinkTag ts


-- | List of tags from the same file
--
data ArbTagsFromFile = ArbTagsFromFile TagFilePath [CTag]
    deriving Show

instance Arbitrary ArbTagsFromFile where
    arbitrary = do
      filePath <- genSmallFilePath
      ArbTagList tags <- arbitrary
      pure $ fixTagsFromFile (ArbTagsFromFile filePath tags)

    shrink (ArbTagsFromFile fp@(TagFilePath rawPath) tags) =
      [ fixTagsFromFile (ArbTagsFromFile fp tags')
      -- Don't shrink file name!
      | tags' <- shrinkList shrinkTag' tags
      ]
      ++
      [ fixTagsFromFile (ArbTagsFromFile (TagFilePath rawPath') tags)
      | rawPath' <- shrink rawPath
      , not (Text.null rawPath')
      ]


-- Assigning one path and clearing fields can make previously distinct tags
-- equal. Both generation and shrinking must restore the duplicate-free input.
fixTagsFromFile :: ArbTagsFromFile -> ArbTagsFromFile
fixTagsFromFile (ArbTagsFromFile filePath tags) =
    ArbTagsFromFile filePath . nub . sortBy CTag.compareTags $
      map (\tag -> tag { tagFilePath = filePath, tagFields = mempty }) tags


--
-- Utils
--

encodeTagFilePath :: TagFilePath -> RawFilePath
encodeTagFilePath = rawFilePathFromBS . Text.encodeUtf8 . getRawFilePath

-- properties

combineTags_subset :: ArbTagsFromFile
                   -> [ArbTag]
                   -> Bool
combineTags_subset (ArbTagsFromFile fp as) bs =
    let bs' = getArbTag `map` bs
        cs = combineTags CTag.compareTags (encodeTagFilePath fp) as bs'
    in all (`elem` cs) as


-- | The tag list be ordered for this property to hold.
--
combineTags_idempotent :: ArbTagsFromFile
                       -> ArbTagList
                       -> Bool
combineTags_idempotent (ArbTagsFromFile fp as) (ArbTagList bs) =
    combineTags CTag.compareTags fp' as bs
    == combineTags CTag.compareTags fp' as
         (combineTags CTag.compareTags fp' as bs)
  where
    fp' = encodeTagFilePath fp


-- | The tag list cannot cannot contain duplicates for this property to hold.
--
combineTags_identity :: ArbTagsFromFile
                     -> Bool
combineTags_identity (ArbTagsFromFile fp as) =
    combineTags CTag.compareTags (encodeTagFilePath fp) as as == as


-- | Does not modify tags outside of the module.
--
combineTags_preserve :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_preserve (ArbTagsFromFile fp as) (ArbTagList bs) =
       filter ((/= fp) . tagFilePath)
              (combineTags CTag.compareTags (encodeTagFilePath fp) as bs)
    ==
       filter ((/= fp) . tagFilePath) bs


-- | Substitutes all tags of the current file.
--
combineTags_substitution :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_substitution (ArbTagsFromFile fp as) (ArbTagList bs) =
       filter (\t -> tagFilePath t == fp) (combineTags CTag.compareTags (encodeTagFilePath fp) as bs)
    ==
       as

-- | 'combineTags' must preserver order of tags.
--
combineTags_order :: ArbTagsFromFile -> ArbTagList -> Bool
combineTags_order (ArbTagsFromFile fp as) (ArbTagList bs) =
    let cs = combineTags CTag.compareTags (encodeTagFilePath fp) as bs
    in sortBy CTag.compareTags cs == cs


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
data ArbTagsFromFileAndTagList = ArbTagsFromFileAndTagList TagFilePath [CTag] [CTag]
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
              . sortBy CTag.compareTags
          <$> listOf tagGen
        tags <- nub
              . sortBy CTag.compareTags
          <$> listOf tagGen
        pure $ ArbTagsFromFileAndTagList filePath tagsFromFile tags
      where
        fixFile p t = t { tagFilePath = p
                        , tagFields   = mempty
                        }

    -- A very basic shrinker
    shrink (ArbTagsFromFileAndTagList filePath@(TagFilePath rawPath) as bs) =
      [ ArbTagsFromFileAndTagList (TagFilePath rawPath')
                                  ((\t -> t { tagFilePath = TagFilePath rawPath' }) `map` as)
                                  bs
      | rawPath' <- shrink rawPath
      , not (Text.null rawPath')
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


-- | Compare the complete streaming merge with the non-streaming merge.
-- Sort both inputs after generation/shrinking, retaining duplicates.
combineTagsStreamProp :: ArbTagsFromFileAndTagList -> Property
combineTagsStreamProp (ArbTagsFromFileAndTagList modPath as bs) =
    combineTags CTag.compareTags modPath' new old
      === runIdentity (Pipes.toListM $
            combineTagsStream CTag.compareTags modPath' new (traverse_ Pipes.yield old))
  where
    modPath' = encodeTagFilePath modPath
    new = sortBy CTag.compareTags as
    old = sortBy CTag.compareTags bs
