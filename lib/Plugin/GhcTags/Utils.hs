module Plugin.GhcTags.Utils
  ( withFileLock
  , LockMode (..)
  ) where

import           Control.Exception
import           System.IO
import           GHC.IO.Handle
import           GHC.IO.Handle.Lock


-- | 'flock' base lock (on posix) or `LockFileEx` on Windows.
--
withFileLock :: FilePath -> LockMode -> IOMode -> (Handle -> IO x) -> IO x
withFileLock path mode iomode k =
    bracket
      (openFile path iomode)
      (\h -> hClose h)
      $ \h ->
        bracket
          (hLock h mode)
          (\_ -> hUnlock h)
          (\_ -> k h)
