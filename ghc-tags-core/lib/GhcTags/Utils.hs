{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GhcTags.Utils
  ( endOfLine
  , notNewLine
  , parseLine
  ) where

import           Control.Applicative ((<|>), optional)
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AChar
import           Data.Functor (($>))
import qualified Data.Text.Encoding as Text


-- | Frame one physical line before parsing it, so a malformed record cannot
-- consume part of the following record. A final unterminated line is passed
-- to the record parser too. Fail only at the end of the input.
parseLine :: Parser a -> Parser (Either String a)
parseLine parser = do
    _ <- AChar.peekChar'
    (line, ()) <- AB.match $
      AChar.takeWhile notNewLine *>
        (     (    AChar.char '\r'
                *> optional (AChar.char '\n')
              ) $> ()
          <|> AChar.char '\n' $> ()
          <|> AB.endOfInput
        )
    let result = case Text.decodeUtf8' line of
          Left err -> Left (show err)
          Right _  -> AB.parseOnly (parser <* AB.endOfInput) line
    pure $ case result of
      Left err    -> Left $ "skipped malformed line \"" ++ take 200 (show line) ++ "\": " ++ err
      Right value -> Right value

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
