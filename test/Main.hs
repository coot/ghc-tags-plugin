module Main where

import           Test.Tasty

import qualified Test.Golden.Parser (tests)


main ::IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Plugin.GhcTags"
    [ Test.Golden.Parser.tests
    ]
