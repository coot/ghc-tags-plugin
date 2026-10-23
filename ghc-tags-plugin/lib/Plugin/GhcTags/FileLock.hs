module Plugin.GhcTags.FileLock
  ( withFileLock_
  , LockMode (..)
  ) where

import           Control.Exception
import           Lukko

-- | 'flock' base lock (on posix) or `LockFileEx` on Windows.
--
withFileLock_ :: FilePath
              -> LockMode
              -> IO a
              -> IO a
withFileLock_ path mode io =
  bracket
    (fdOpen path)
    fdClose
    $ \h ->
      -- lock the file using internal bracket, so if `fdLock` throws, we will
      -- still close the file handle.
      bracket_
        (fdLock h mode)
        (fdUnlock h)
        io
