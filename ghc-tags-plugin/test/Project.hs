{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (filterM, forM_, when)
import Data.List (find, scanl')
import Data.Maybe (isJust)
import System.Directory
import System.FilePath ((</>), normalise, splitDirectories, takeDirectory, takeExtension)
import System.IO (hClose, openTempFile)

import Test.Tasty (defaultMain)
import Test.Tasty.QuickCheck

import Plugin.GhcTags.Project


main :: IO ()
main = defaultMain $ testProperty "project-relative tags path matches model" resolutionProp


data ProjectConfig = CabalProject
                   | CabalProjectLocal
                   | StackConfig
  deriving (Eq, Show)

data DirectoryEntry = DirectoryEntry (Maybe ProjectConfig) Bool
  deriving (Eq, Show)

instance Arbitrary DirectoryEntry where
    arbitrary = DirectoryEntry
      <$> frequency
            [ (6, pure Nothing)
            , (1, pure (Just CabalProject))
            , (1, pure (Just CabalProjectLocal))
            , (1, pure (Just StackConfig))
            ]
      <*> arbitrary
    shrink (DirectoryEntry config package) =
         [ DirectoryEntry Nothing package | isJust config ]
      ++ [ DirectoryEntry config False | package ]


data StackOverride = NoOverride
                   | ExistingOverride
                   | MissingOverride
  deriving (Eq, Show)

instance Arbitrary StackOverride where
    arbitrary = frequency
                  [ (6, pure NoOverride)
                  , (2, pure ExistingOverride)
                  , (1, pure MissingOverride)
                  ]
    shrink NoOverride = []
    shrink _ = [NoOverride]


data ProjectLayout = ProjectLayout {
    levels         :: NonEmptyList DirectoryEntry,
    absoluteOutput :: Bool,
    stackOverride  :: StackOverride,
    relativeOutput :: FilePath
  }
  deriving Show

instance Arbitrary ProjectLayout where
    arbitrary = ProjectLayout
      <$> resize 6 arbitrary
      <*> frequency
            [ (7, pure False)
            , (1, pure True)
            ]
      <*> arbitrary
      <*> elements ["tags", "TAGS", "output/tags", "../tags"]
    shrink layout@ProjectLayout {levels, absoluteOutput, stackOverride} =
         [ layout { levels = levels' }
         | levels' <- shrink levels ]
      ++ [ layout { absoluteOutput = False }
         | absoluteOutput ]
      ++ [ layout { stackOverride = stackOverride' }
         | stackOverride' <- shrink stackOverride ]


resolutionProp :: ProjectLayout -> Property
resolutionProp ProjectLayout {levels = NonEmpty entries, absoluteOutput, stackOverride, relativeOutput} =
    ioProperty $ withTestDirectory $ \root -> do
      let components = ["level" ++ show n | n <- [0 .. length entries - 1]]
          directories = drop 1 $ scanl' (</>) root components
          cwd = foldl (</>) root components
          overrideRoot = root </> "override"
          overrideFile = overrideRoot </> "custom-stack.yaml"
          override = case stackOverride of
            NoOverride -> Nothing
            _ -> Just overrideFile
          path = if absoluteOutput then root </> "absolute.tags" else relativeOutput
      createDirectoryIfMissing True cwd
      forM_ (zip directories entries) $ \(dir, DirectoryEntry config package) -> do
        forM_ config $ \kind -> writeFile (dir </> configName kind) ""
        when package $ writeFile (dir </> "package.cabal") ""
      when (stackOverride == ExistingOverride) $ do
        createDirectory overrideRoot
        writeFile overrideFile ""
      -- The resolver can leave the generated hierarchy. Include ambient
      -- directories in the model when no generated project marker stops it.
      ancestors <-
        if   absoluteOutput
          || stackOverride == ExistingOverride
          || any (\(DirectoryEntry config _) -> isJust config) entries
          then pure []
          else readAncestorEntries root
      actual <- resolveTagsFile cwd override path
      let -- Independent model: select the innermost matching directory from
          -- the generated hierarchy followed by its surrounding directories.
          innerFirst = reverse (zip directories entries) ++ ancestors
          project = fst <$> find (\(_, DirectoryEntry config _) -> isJust config) innerFirst
          package = fst <$> find (\(_, DirectoryEntry _ present) -> present) innerFirst

          fallback = maybe cwd id package

          expected :: TagsAtRoot
          expected
            | absoluteOutput
            = (TagsAtProjectRoot path)

            | ExistingOverride <- stackOverride
            = TagsAtProjectRoot (normalise (overrideRoot </> path))

            | Just dir <- project
            = TagsAtProjectRoot (normalise (dir </> path))

            | otherwise
            = TagsAtPackageRoot (normalise (fallback </> path))

      pure $
        classify absoluteOutput "absolute output" $
        classify (stackOverride == ExistingOverride) "explicit Stack configuration" $
        classify (stackOverride == MissingOverride) "missing Stack configuration" $
        classify (not absoluteOutput && stackOverride /= ExistingOverride && not (isJust project)) "fallback" $
          actual === expected


configName :: ProjectConfig -> FilePath
configName CabalProject      = "cabal.project"
configName CabalProjectLocal = "cabal.project.local"
configName StackConfig       = "stack.yaml"


-- Snapshot filesystem data only; root selection stays in the property model.
readAncestorEntries :: FilePath -> IO [(FilePath, DirectoryEntry)]
readAncestorEntries root = traverse readEntry parents
  where
    parents = reverse . drop 1 . scanl' (</>) "" . splitDirectories . takeDirectory $ root

    readEntry dir = do
      files <- filterM (doesFileExist . (dir </>)) =<< listDirectory dir
      let config = find ((`elem` files) . configName)
                 $ [CabalProject, CabalProjectLocal, StackConfig]
          package = any ((== ".cabal") . takeExtension) files
      pure (dir, DirectoryEntry config package)


withTestDirectory :: (FilePath -> IO a) -> IO a
withTestDirectory action = do
    tmp <- getTemporaryDirectory
    bracket (do (path, h) <- openTempFile tmp "ghc-tags-project-test"
                hClose h
                removeFile path
                createDirectory path
                canonicalizePath path)
            removeDirectoryRecursive
            action
