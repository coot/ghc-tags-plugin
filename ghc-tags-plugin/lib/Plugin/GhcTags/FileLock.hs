{-# LANGUAGE CPP #-}

module Plugin.GhcTags.FileLock
  ( withFileLock
  , LockMode (..)
  ) where

import           Control.Exception

#if !defined(mingw32_HOST_OS)
import           Lukko.FLock
#else
import           Lukko.Windows
#endif

-- | 'flock' base lock (on posix) or `LockFileEx` on Windows.
--
withFileLock :: FilePath -> LockMode -> (FD -> IO x) -> IO x
withFileLock path mode k =
    bracket
      (fdOpen path)
      (\h -> fdClose h)
      $ \h ->
        bracket
          (fdLock h mode)
          (\_ -> fdUnlock h)
          (\_ -> k h)
