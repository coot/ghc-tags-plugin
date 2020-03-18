{-# LANGUAGE CPP #-}

module Plugin.GhcTags.Utils
  ( endOfLine

  , withFileLock
  , LockMode (..)
  ) where

import           Control.Exception
import           System.IO
import           GHC.IO.Handle
import           GHC.IO.Handle.Lock


-- | Platform dependend eol:
--
-- * windows      "CRNL"
-- * maxos        "CR"
-- * linux (unit) "NL"
--
endOfLine :: String
#if defined(mingw32_HOST_OS)
endOfLine = "\r\n"
#elif defined(darwin_HIST_OS)
endOfLine = "\r"
#else
endOfLine = "\n"
#endif


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
