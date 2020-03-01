{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

module Plugin.GhcTags.Parser
  ( TagName (..)
  , TagFile (..)
  , Tag (..)
  , parseVimTagFile
  , TagsMap
  , mkTagsMap
  ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)
import           Data.Attoparsec.ByteString               ( Parser )
import qualified Data.Attoparsec.ByteString       as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto ( decimal
                                                          , endOfLine
                                                          )
import           Data.List ( sortOn )
import           Data.Map  ( Map )
import qualified Data.Map as Map

newtype TagName = TagName { getTagName :: ByteString }
  deriving newtype (Eq, Ord, Show)

newtype TagFile = TagFile { getTagFile :: ByteString }
  deriving newtype (Eq, Ord, Show)

data Tag = Tag
  { tagName :: !TagName
  , tagFile :: !TagFile
  , tagLine :: !Int
  }
  deriving Show

vimTagLineParser:: Parser Tag
vimTagLineParser =
    Tag <$> (TagName <$> Atto.takeWhile (/= tab) <* Atto.skipWhile (== tab))
        <*> (TagFile <$> Atto.takeWhile (/= tab) <* Atto.skipWhile (== tab))
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

type TagsMap = Map TagFile [Tag]

-- | Map from TagName to list of tags.  This will be useful when updating tags.
-- We will just need to merge dictionaries.
--
mkTagsMap :: [Tag] -> TagsMap
mkTagsMap =
      fmap (sortOn (\t -> (tagFile t, tagLine t)))
    . Map.fromListWith (<>)
    . map (\t -> (tagFile t, [t]))
