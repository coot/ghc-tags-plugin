module Plugin.GhcTags.AtomicFile (withAtomicFile) where

import Control.Exception (bracket, finally)
import Control.Monad (when)
import System.Directory (canonicalizePath, copyPermissions, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)

-- | Build a replacement beside the destination, then publish it with a rename.
-- The caller must hold the destination's lock for the whole read/modify/write.
-- Returning False leaves the original untouched (e.g. after a parse error).
--
-- If @path@ is a symlink the rename updates the symlink's target, not the
-- symlink itself, so a shared tags file stays in sync with the configured
-- path.
withAtomicFile :: FilePath -> (Handle -> IO Bool) -> IO ()
withAtomicFile path write = do
    -- Resolve symlinks so the temp file lands beside the real destination and
    -- the final rename updates the target rather than replacing the symlink.
    -- Falls back to the original path on the first write (file doesn't exist).
    dest <- canonicalizePath path `catchIOError` \_ -> pure path
    bracket
      (openBinaryTempFile (takeDirectory dest) ("." ++ takeFileName dest ++ ".tmp"))
      (\(tmp, h) -> hClose h `finally` removeTemp tmp)
      (\(tmp, h) -> do
          commit <- write h
          -- Close before renaming; this is subtle.  If `hClose` throws, we
          -- won't overwrite the `dest`, if we'd just use `hFlush` instead, it
          -- could succeed, then we'd overwrite the `dest` and `hClose` and
          -- `hClose` fail afterwards.
          hClose h
          when commit $ do
            -- Preserve the destination's permissions on the replacement.
            -- Silently skip when the destination doesn't exist yet (first write).
            copyPermissions dest tmp `catchIOError` \err ->
              if isDoesNotExistError err then pure () else ioError err
            renameFile tmp dest)
  where
    removeTemp tmp = removeFile tmp `catchIOError` \err ->
      if isDoesNotExistError err then pure () else ioError err
