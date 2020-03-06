module Main where

import           Test.Tasty

import qualified Test.Golden.Parser (tests)
import qualified Test.Vim           (tests)


main ::IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Plugin.GhcTags"
    [ Test.Golden.Parser.tests
    , Test.Vim.tests
    ]
