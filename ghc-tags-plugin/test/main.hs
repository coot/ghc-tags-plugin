module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.AtomicFile
import qualified Test.Check
import qualified Test.ProjectFile


main :: IO ()
main = defaultMain $ testGroup "ghc-tags-plugin"
    [ Test.AtomicFile.tests
    , Test.Check.tests
    , Test.ProjectFile.tests
    ]
