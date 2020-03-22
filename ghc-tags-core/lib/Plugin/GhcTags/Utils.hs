{-# LANGUAGE CPP #-}

module Plugin.GhcTags.Utils
  ( endOfLine
  , notNewLine
  ) where

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


notNewLine :: Char -> Bool
notNewLine = \x -> x /= '\n' && x /= '\r'
