module Test.Options (tests) where

import Data.Maybe (maybeToList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (Success)

import Plugin.GhcTags.Options


tests :: TestTree
tests = testGroup "Options"
    [ testProperty "filtering defaults on and accepts both disabling flags" filterOptionsProp ]

data FilterFlag = NoArg | LongArg | ShortArg
  deriving (Show, Eq)

instance Arbitrary FilterFlag where
  arbitrary = elements [NoArg, LongArg, ShortArg]
  shrink LongArg  = [ShortArg, NoArg]
  shrink ShortArg = [NoArg]
  shrink NoArg    = []


filterFlagToFlag :: FilterFlag -> Maybe String
filterFlagToFlag LongArg  = Just "--no-filter"
filterFlagToFlag ShortArg = Just "-F"
filterFlagToFlag NoArg    = Nothing

filterFlagToFilter :: FilterFlag -> Filter
filterFlagToFilter LongArg  = RetainAll
filterFlagToFilter ShortArg = RetainAll
filterFlagToFilter NoArg    = RemoveTerms


filterOptionsProp :: FilterFlag -> Property
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
