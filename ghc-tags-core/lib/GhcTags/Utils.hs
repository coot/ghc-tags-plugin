{-# LANGUAGE CPP #-}

module GhcTags.Utils
  ( endOfLine
  , notNewLine
  ) where

-- | Platform dependent eol:
--
-- * Windows      "CRNL"
-- * MacOS        "CR"
-- * Linux (unit) "NL"
--
endOfLine :: String
#if defined(mingw32_HOST_OS)
endOfLine = "\r\n"
#elif defined(darwin_HIST_OS)
endOfLine = "\r"
#else
endOfLine = "\n"
#endif


notNewLine :: Char -> Bool
notNewLine = \x -> x /= '\n' && x /= '\r'
