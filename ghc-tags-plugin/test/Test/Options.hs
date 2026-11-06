module Test.Options (tests) where

import Data.Maybe (maybeToList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (Success)

import Plugin.GhcTags.Options


tests :: TestTree
tests = testGroup "Options"
    [ testProperty "filtering defaults on and accepts both disabling flags" filterOptionsProp
    , testProperty "pattern matches default off and accept both enabling flags" patternMatchesOptionsProp
    ]

-- | How a switch is passed: not at all, or using its long or short form.
data SwitchArg = NoArg | LongArg | ShortArg
  deriving (Show, Eq)

instance Arbitrary SwitchArg where
  arbitrary = elements [NoArg, LongArg, ShortArg]
  shrink LongArg  = [ShortArg, NoArg]
  shrink ShortArg = [NoArg]
  shrink NoArg    = []


filterFlagToFlag :: SwitchArg -> Maybe String
filterFlagToFlag LongArg  = Just "--no-filter"
filterFlagToFlag ShortArg = Just "-F"
filterFlagToFlag NoArg    = Nothing

filterFlagToFilter :: SwitchArg -> Filter
filterFlagToFilter LongArg  = RetainAll
filterFlagToFilter ShortArg = RetainAll
filterFlagToFilter NoArg    = RemoveTerms


filterOptionsProp :: SwitchArg -> Property
filterOptionsProp flag =
    let args = maybeToList (filterFlagToFlag flag)
    in checkCoverage
       $ cover 10 (flag == NoArg) "default"
       $ cover 10 (flag == LongArg) "long flag"
       $ cover 10 (flag == ShortArg) "short flag"
       $ case runOptionParser args of
           Success options ->
             filterOption options
             ===
             filterFlagToFilter flag
           _ -> counterexample ("Failed to parse " ++ show args) False


patternMatchesFlagToFlag :: SwitchArg -> Maybe String
patternMatchesFlagToFlag LongArg  = Just "--pattern-matches"
patternMatchesFlagToFlag ShortArg = Just "-p"
patternMatchesFlagToFlag NoArg    = Nothing

patternMatchesFlagToPatternMatches :: SwitchArg -> PatternMatches
patternMatchesFlagToPatternMatches LongArg  = IncludePatternMatches
patternMatchesFlagToPatternMatches ShortArg = IncludePatternMatches
patternMatchesFlagToPatternMatches NoArg    = ExcludePatternMatches


patternMatchesOptionsProp :: SwitchArg -> Property
patternMatchesOptionsProp flag =
    let args = maybeToList (patternMatchesFlagToFlag flag)
    in checkCoverage
       $ cover 10 (flag == NoArg) "default"
       $ cover 10 (flag == LongArg) "long flag"
       $ cover 10 (flag == ShortArg) "short flag"
       $ case runOptionParser args of
           Success options ->
             patternMatches options
             ===
             patternMatchesFlagToPatternMatches flag
           _ -> counterexample ("Failed to parse " ++ show args) False
