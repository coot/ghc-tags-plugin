module Plugin.GhcTags.Project
  ( TagsAtRoot (..)
  , resolveTagsFile
  ) where

import Data.Maybe (fromMaybe)
import System.Directory (canonicalizePath, doesFileExist, listDirectory)
import System.FilePath (isAbsolute, normalise, takeDirectory, takeExtension, (</>))


-- Resolved tag file path
data TagsAtRoot = TagsAtProjectRoot FilePath
                | TagsAtPackageRoot FilePath
  deriving (Show, Eq)


-- | Resolve an output path from the compiler's working directory and optional
-- STACK_YAML setting. A fallback directory is returned when no project
-- configuration was found, so the caller can report a warning.
resolveTagsFile :: FilePath
                -- ^ current working directory
                -> Maybe FilePath
                -- ^ `stack.yaml` absolute path (read from `STACK_YAML`
                -- variable)
                -> FilePath
                -- ^ user supplied tags file path
                -> IO TagsAtRoot
                -- ^ tuple of resolved patch and a fallback 
resolveTagsFile _ _ path | isAbsolute path = pure (TagsAtProjectRoot path)
resolveTagsFile cwd stackYaml path = do
    start <- canonicalizePath cwd
    stackRoot <- case stackYaml of
      Just config | not (null config) -> do
        let configPath = cwd </> config
        exists <- doesFileExist configPath
        if exists
          then Just . takeDirectory <$> canonicalizePath configPath
          else pure Nothing
      _ -> pure Nothing
    case stackRoot of
      Just root -> pure (TagsAtProjectRoot $ normalise (root </> path))
      Nothing -> search start Nothing start
  where
    search start packageRoot dir = do
      configs <- traverse (doesFileExist . (dir </>))
                   [ "cabal.project"
                   , "cabal.project.local"
                   , "stack.yaml"
                   ]
      if or configs
        -- found project config
        then pure (TagsAtProjectRoot $ normalise (dir </> path))
        else do
          -- look for `*.cabal` files
          packageRoot' <- case packageRoot of
            Just _ -> pure packageRoot
            Nothing -> do
              entries <- listDirectory dir
              packages <- traverse (doesFileExist . (dir </>))
                (filter ((== ".cabal") . takeExtension) entries)
              pure $ if or packages then Just dir else Nothing
          let parent = takeDirectory dir
          if parent == dir
            then do
              -- We reached file system root directory, resolve tags file
              -- relative to last found package root.
              let root = fromMaybe start packageRoot'
              pure (TagsAtPackageRoot $ normalise (root </> path))
            else
              -- continue at the parent directory
              search start packageRoot' parent
