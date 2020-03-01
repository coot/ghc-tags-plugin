{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

module Plugin.GhcTags.Parser
  ( -- * Tag
    TagName (..)
  , TagFile (..)
  , Tag (..)
    -- * Parsing
  , parseVimTagFile
    -- * TagsMap
  , TagsMap
  , mkTagsMap
  ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)
import           Data.Attoparsec.ByteString               (Parser)
import qualified Data.Attoparsec.ByteString       as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto ( decimal
                                                          , endOfLine
                                                          )
import           Data.List (sort)
import           Data.Map  (Map)
import qualified Data.Map as Map


--
-- Tag
--


-- | 'ByteString' which encodes a tag name.
--
newtype TagName = TagName { getTagName :: ByteString }
  deriving newtype (Eq, Ord, Show)


-- | 'ByteString' which encodes a tag file.
--
newtype TagFile = TagFile { getTagFile :: ByteString }
  deriving newtype (Eq, Ord, Show)


-- | Simple Tag record.  For the moment on tag name, tag file and line numbers
-- are supported.
--
-- TODO: expand to support column numbers and extra information.
--
data Tag = Tag
  { tagName :: !TagName
  , tagFile :: !TagFile
  , tagLine :: !Int
  }
  deriving (Ord, Eq, Show)


--
-- Parsing
--


-- | Parser for a single line of a vim-style tag file.
--
vimTagLineParser:: Parser Tag
vimTagLineParser =
    Tag <$> (TagName <$> Atto.takeWhile (/= tab) <* Atto.skipWhile (== tab))
        <*> (TagFile <$> Atto.takeWhile (/= tab) <* Atto.skipWhile (== tab))
        <*> Atto.decimal
  where
    tab = c2w '\t'

-- | A vim-style tag file parser.
--
vimTagFileParser :: Parser [Tag]
vimTagFileParser = Atto.sepBy vimTagLineParser Atto.endOfLine


-- | Parse a vim-style tag file.
--
parseVimTagFile :: ByteString
                -> IO (Either String [Tag])
parseVimTagFile =
      fmap Atto.eitherResult
    . Atto.parseWith (pure mempty) vimTagFileParser


--
-- TagsMap
--


type TagsMap = Map TagFile [Tag]

-- | Map from TagName to list of tags.  This will be useful when updating tags.
-- We will just need to merge dictionaries.
--
mkTagsMap :: [Tag] -> TagsMap
mkTagsMap =
      fmap sort
    . Map.fromListWith (<>)
    . map (\t -> (tagFile t, [t]))
