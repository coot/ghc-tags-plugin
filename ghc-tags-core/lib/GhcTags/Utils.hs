{-# LANGUAGE CPP #-}

module GhcTags.Utils
  ( endOfLine
  , notNewLine
  ) where

-- | Platform dependent eol:
--
-- * Windows        "CRNL"
-- * Linux & Darwin "NL"
--
endOfLine :: String
#if defined(mingw32_HOST_OS)
endOfLine = "\r\n"
#else
endOfLine = "\n"
#endif


notNewLine :: Char -> Bool
notNewLine = \x -> x /= '\n' && x /= '\r'
