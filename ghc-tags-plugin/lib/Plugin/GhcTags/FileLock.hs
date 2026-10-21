module Plugin.GhcTags.FileLock
  ( withFileLock_
  , LockMode (..)
  ) where

import           Control.Exception
import           Control.Monad (when)

import           Lukko

-- | 'flock' base lock (on posix) or `LockFileEx` on Windows.
--
withFileLock_ :: Bool -- ^ debug option
              -> FilePath
              -> LockMode
              -> IO a
              -> IO a
withFileLock_ debug path mode io =
  bracket
    (fdOpen path)
    fdClose
    $ \h ->
      -- lock the file using internal bracket, so if `fdLock` throws, we will
      -- still close the file handle.
      bracket_
        (do fdLock h mode
            when debug (putStrLn "lock: taken"))
        (do fdUnlock h
            when debug (putStrLn "lock: released"))
        io
