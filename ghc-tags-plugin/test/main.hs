module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.AtomicFile
import qualified Test.Check
import qualified Test.Ghc
import qualified Test.Options
import qualified Test.ProjectFile
import qualified Test.Timing


main :: IO ()
main = defaultMain $ testGroup "ghc-tags-plugin"
    [ Test.AtomicFile.tests
    , Test.Check.tests
    , Test.Ghc.tests
    , Test.Options.tests
    , Test.ProjectFile.tests
    , Test.Timing.tests
    ]
