{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Test.Tag (tests) where

import           Data.Function (on)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           Plugin.GhcTags.Tag

import           Test.Tag.Generators

tests :: TestTree
tests = testGroup "Tag"
  [ testProperty "Ord:antisymmetry" ordAntiSymmetryProp
  , testProperty "Ord:reflexivity"  (on ordReflexivityyProp getArbOrdTag)
  , testProperty "Ord:transitivity" (\a b c ->
                                      ordTransitiveProp
                                        (getArbOrdTag a)
                                        (getArbOrdTag b)
                                        (getArbOrdTag c))
  , testProperty "Ord:Eq:consistency" (weakConsistency . getArbOrdTag)
  ]


-- | Arbitrary instance with a high probability of gettings the same tags or files.
newtype ArbOrdTag = ArbOrdTag { getArbOrdTag :: Tag }
  deriving Show

instance Arbitrary ArbOrdTag where
    arbitrary = fmap ArbOrdTag
              $  Tag
             <$> elements (TagName `map`
                       [ "find"
                       , "Ord"
                       , "Eq"
                       , "hPut"
                       ])
             <*> genTagKind
             <*> elements (TagFile `map`
                       [ "Main.hs"
                       , "Lib.hs"
                       ])
             <*> frequency
                   [ (8, Left . getPositive <$> arbitrary)
                   , (1, Right . (wrap '/' . fixAddr) <$> genTextNonEmpty)
                   , (1, Right . (wrap '?' . fixAddr) <$> genTextNonEmpty)
                   ]
             <*> pure []


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


-- | The
--
-- prop> a == b ==> a `compare` b == EQ`
--
-- But since 'Tag' is using derived 'Eq' instance, it is equivalent to
weakConsistency :: Tag -> Bool
weakConsistency a = a `compareTags` a == EQ
