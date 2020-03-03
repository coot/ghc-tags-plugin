{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Plugin.GhcTags.Parser
  ( -- * Tag
    TagName (..)
  , TagFile (..)
  , Tag (..)
  , ghcTagToTag
    -- * Parsing
  , parseVimTagFile
    -- * TagsMap'
  , TagsMap
  , mkTagsMap
  ) where

import           Control.Applicative (many)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Attoparsec.ByteString               (Parser)
import qualified Data.Attoparsec.ByteString       as A
import           Data.Attoparsec.ByteString.Char8      ( (<?>) )
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.Either (rights)
import           Data.List (sort)
import           Data.Functor (void)
import           Data.Map  (Map)
import qualified Data.Map as Map

-- GHC imports
import           Plugin.GhcTags.Generate
                              ( GhcTag (..)
                              , TagKind
                              , charToTagKind
                              )
import           FastString   ( FastString (..)
                              )
import           SrcLoc       ( SrcSpan (..)
                              , srcSpanFile
                              , srcSpanStartLine
                              )


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
  , tagKind :: !(Maybe TagKind)
  }
  deriving (Ord, Eq, Show)


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { gtSrcSpan, gtTag, gtKind } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName = TagName (fs_bs gtTag)
                   , tagFile = TagFile (fs_bs (srcSpanFile realSrcSpan))
                   , tagLine = srcSpanStartLine realSrcSpan
                   , tagKind = Just gtKind
                   }


--
-- Parsing
--


-- | Parser for a single line of a vim-style tag file.
--
vimTagParser:: Parser Tag
vimTagParser = do
    -- use monadic form to provide compatibility with previous version where
    -- `;"` and tag kinds where not present.
    tagName <-
      TagName <$> AC.takeWhile (/= '\t') <* AC.skipWhile (== '\t')
              <?> "parsing tag name failed"
    tagFile <-
      TagFile <$> AC.takeWhile (/= '\t') <* AC.skipWhile (== '\t')
              <?> "parsing tag file name failed"
    tagLine <-    AC.decimal
              <?> "parsing line number failed"
      
    mc <- AC.peekChar
    tagKind <-
      case mc of
        Just ';' ->
                charToTagKind
            <$> (AC.anyChar *> AC.char '"' *> AC.char '\t' *> AC.anyChar)
            <?> "parsing tag kind failed"
        _  -> pure Nothing
    AC.endOfLine
    pure $ Tag {tagName, tagFile, tagLine, tagKind}


-- | A vim-style tag file parser.
--
vimTagFileParser :: Parser [Tag]
vimTagFileParser = rights <$> many tagLineParser


tagLineParser :: Parser (Either () Tag)
tagLineParser =
    AC.eitherP
      (vimTagHeaderLine <?> "failed parsing tag")
      (vimTagParser     <?> "failed parsing header")


vimTagHeaderLine :: Parser ()
vimTagHeaderLine = AC.choice
    [ AC.string (BSC.pack "!_TAG_FILE_FORMAT")     *> params
    , AC.string (BSC.pack "!_TAG_FILE_SORTED")     *> params
    , AC.string (BSC.pack "!_TAG_FILE_ENCODING")   *> params
    , AC.string (BSC.pack "!_TAG_PROGRAM_AUTHOR")  *> params
    , AC.string (BSC.pack "!_TAG_PROGRAM_NAME")    *> params
    , AC.string (BSC.pack "!_TAG_PROGRAM_URL")     *> params
    , AC.string (BSC.pack "!_TAG_PROGRAM_VERSION") *> params
    ]
  where

    params = void $ AC.char '\t' *> AC.skipWhile (/= '\n') *> AC.char '\n'

-- | Parse a vim-style tag file.
--
parseVimTagFile :: ByteString
                -> IO (Either String [Tag])
parseVimTagFile =
      fmap A.eitherResult
    . A.parseWith (pure mempty) vimTagFileParser


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
