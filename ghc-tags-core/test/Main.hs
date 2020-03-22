module Main where

import           Control.Exception
import           Data.Bool
import           Data.Monoid

import           Test.Tasty

import qualified Test.Golden.Parser (tests)
import qualified Test.Tag           (tests)
import qualified Test.CTags         (tests)
import qualified Test.ETags         (tests)

import           System.Directory


main ::IO ()
main = do
    -- useing 'IO' 'Monoid' instance
    mGoldenDir
      <- doesGoldenDirectoryExist "test/golden"
      <> doesGoldenDirectoryExist "ghc-tags-core/test/golden"

    case mGoldenDir :: First FilePath of
      First Nothing ->
        throwIO $ userError "no 'test/golden' directory found"

      First (Just goldenDir) ->
        defaultMain (tests goldenDir)
  where
    fromBool :: FilePath -> Bool -> First FilePath
    fromBool = bool mempty . First  . Just

    doesGoldenDirectoryExist :: FilePath -> IO (First FilePath)
    doesGoldenDirectoryExist fp = fromBool fp <$> doesDirectoryExist fp


tests :: FilePath -> TestTree
tests goldenTestDir =
    testGroup "Plugin.GhcTags"
    [ Test.Golden.Parser.tests goldenTestDir
    , Test.Tag.tests
    , Test.CTags.tests
    , Test.ETags.tests
    ]
