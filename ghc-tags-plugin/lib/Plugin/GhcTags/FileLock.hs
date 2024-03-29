{-# LANGUAGE CPP #-}

module Plugin.GhcTags.FileLock
  ( withFileLock
  , LockMode (..)
  ) where

import           Control.Exception
import           Control.Monad (when)

#if !defined(mingw32_HOST_OS)
import           Lukko.FLock
#else
import           Lukko.Windows
#endif

-- | 'flock' base lock (on posix) or `LockFileEx` on Windows.
--
withFileLock :: Bool -- ^ debug option
             -> FilePath -> LockMode -> (FD -> IO x) -> IO x
withFileLock debug path mode k =
    bracket
      (fdOpen path)
      (\h -> fdClose h)
      $ \h ->
        bracket
          (do fdLock h mode
              when debug (putStrLn "lock: taken"))
          (\_ ->
           do when debug (putStrLn "lock: releasing")
              fdUnlock h)
          (\_ -> k h)
