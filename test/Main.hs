module Main where

import           Test.Tasty

import qualified Test.Golden.Parser (tests)
import qualified Test.Tag           (tests)
import qualified Test.CTags         (tests)


main ::IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Plugin.GhcTags"
    [ Test.Golden.Parser.tests
    , Test.Tag.tests
    , Test.CTags.tests
    ]
