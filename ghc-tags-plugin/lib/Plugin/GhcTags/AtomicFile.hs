module Plugin.GhcTags.AtomicFile (withAtomicFile) where

import Control.Exception (bracket, finally)
import Control.Monad (when)
import System.Directory (copyPermissions, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)

-- | Build a replacement beside the destination, then publish it with a rename.
-- The caller must hold the destination's lock for the whole read/modify/write.
-- Returning False leaves the original untouched (e.g. after a parse error).
withAtomicFile :: FilePath -> (Handle -> IO Bool) -> IO ()
withAtomicFile path write =
    bracket
      (openBinaryTempFile (takeDirectory path) ("." ++ takeFileName path ++ ".tmp"))
      (\(tmp, h) -> hClose h `finally` removeTemp tmp)
      (\(tmp, h) -> do
          commit <- write h
          -- Close before renaming; this is subtle.  If `hClose` throws, we
          -- won't overwrite the `path`, if we'd just use `hFlush` instead, it
          -- could succeed, then we'd overwrite the `path` and `hClose` and
          -- `hClose` fail afterwards. 
          hClose h
          when commit $ do
            -- Preserve the destination's permissions on the replacement.
            -- Silently skip when the destination doesn't exist yet (first write).
            copyPermissions path tmp `catchIOError` \err ->
              if isDoesNotExistError err then pure () else ioError err
            renameFile tmp path)
  where
    removeTemp tmp = removeFile tmp `catchIOError` \err ->
      if isDoesNotExistError err then pure () else ioError err
