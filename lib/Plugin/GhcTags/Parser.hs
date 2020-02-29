module Plugin.GhcTags.Parser
  ( Tag (..)
  , parseVimTagFile
  ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)
import           Data.Attoparsec.ByteString               ( Parser )
import qualified Data.Attoparsec.ByteString       as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto ( decimal
                                                          , endOfLine
                                                          )

data Tag = Tag
  { tag     :: ByteString
  , tagFile :: ByteString
  , tagLine :: Int
  }
  deriving Show

vimTagLineParser:: Parser Tag
vimTagLineParser =
    Tag <$> (Atto.takeWhile (/= tab) <* Atto.skipWhile (== tab))
        <*> (Atto.takeWhile (/= tab) <* Atto.skipWhile (== tab))
        <*> Atto.decimal
  where
    tab = c2w '\t'

vimTagFileParser :: Parser [Tag]
vimTagFileParser = Atto.sepBy vimTagLineParser Atto.endOfLine

parseVimTagFile :: ByteString
                -> IO (Either String [Tag])
parseVimTagFile =
      fmap Atto.eitherResult
    . Atto.parseWith (pure mempty) vimTagFileParser
