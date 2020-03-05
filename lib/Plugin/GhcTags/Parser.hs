{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Plugin.GhcTags.Parser
  ( -- * Tag
    Tag (..)
  , TagName (..)
  , TagFile (..)
  , TagField (..)
  , ghcTagToTag
    -- * Parsing
  , parseVimTagFile
    -- * TagsMap'
  , TagsMap
  , mkTagsMap
  ) where

import           Control.Applicative (many)
import           Data.Attoparsec.Text  (Parser, (<?>))
import qualified Data.Attoparsec.Text  as AT
import           Data.Either (rights)
import           Data.List (sort)
import           Data.Functor (void)
import           Data.Map  (Map)
import qualified Data.Map as Map
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

-- GHC imports
import           Plugin.GhcTags.Generate
                              ( GhcTag (..)
                              , TagKind
                              , TagField (..)
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
newtype TagName = TagName { getTagName :: Text }
  deriving newtype (Eq, Ord, Show)


-- | 'ByteString' which encodes a tag file.
--
newtype TagFile = TagFile { getTagFile :: Text }
  deriving newtype (Eq, Ord, Show)


-- | Simple Tag record.  For the moment on tag name, tag file and line numbers
-- are supported.
--
-- TODO: expand to support column numbers and extra information.
--
data Tag = Tag
  { tagName   :: !TagName
  , tagFile   :: !TagFile
  , tagAddr   :: !(Either Int Text)
  , tagKind   :: !(Maybe TagKind)
  , tagFields :: ![TagField]
  }
  deriving (Ord, Eq, Show)


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { gtSrcSpan, gtTag, gtKind, gtFields } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName   = TagName (Text.decodeUtf8 $ fs_bs gtTag)
                   , tagFile   = TagFile (Text.decodeUtf8 $ fs_bs (srcSpanFile realSrcSpan))
                   , tagAddr   = Left (srcSpanStartLine realSrcSpan)
                   , tagKind   = Just gtKind
                   , tagFields = gtFields
                   }


--
-- Parsing
--


-- | Parser for a single line of a vim-style tag file.
--
vimTagParser :: Parser Tag
vimTagParser =
        Tag
    <$> parseTagName
    <*  separator

    <*> parseTagFile
    <*  separator

    -- includes an optional ';"' separator
    <*> AT.eitherP parseAddr parseExCommand
    <*  separator

    <*> (either kindFromField id
          <$> AT.eitherP
                parseField
                (charToTagKind <$> AT.satisfy notTabOrNewLine))

    <*> (either (const []) id
          <$> AT.eitherP 
                AT.endOfLine
                (separator
                  *> AT.sepBy parseField separator
                   <* AT.endOfLine))
  where
    separator = AT.char '\t'
    notTabOrNewLine = \x -> x /= 't' && x /= '\n' 

    parseTagName :: Parser TagName
    parseTagName = TagName <$> AT.takeWhile (/= '\t')
                           <?> "parsing tag name failed"

    parseTagFile :: Parser TagFile
    parseTagFile = TagFile <$> AT.takeWhile (/= '\t')

    parseExCommand :: Parser Text
    parseExCommand = (\x -> Text.take (Text.length x - 1) x) <$>
                     AT.scan "" go
                  <* AT.anyChar
      where
        -- go until either '`n' or ';"' sequence is found.
        go :: String -> Char -> Maybe String
        go _ '\n'             = Nothing
        go !s c  | l == "\";" = Nothing
                 | otherwise  = Just l
          where
            l = take 2 (c : s)

    parseAddr :: Parser Int
    parseAddr = AT.decimal
             <* AT.eitherP
                  AT.endOfLine
                  (AT.char ';' *> AT.char '"')

    kindFromField :: TagField -> Maybe TagKind
    kindFromField TagField { fieldName = "kind", fieldValue } =
        if Text.null fieldValue
          then Nothing
          else charToTagKind (Text.head fieldValue)
    kindFromField _ = Nothing


parseField :: Parser TagField
parseField =
         TagField
     <$> AT.takeWhile (\x -> x /= ':' && x /= '\n')
     <*  AT.char ':'
     <*> AT.takeWhile (\x -> x /= '\t' && x /= '\n')


-- | A vim-style tag file parser.
--
vimTagFileParser :: Parser [Tag]
vimTagFileParser = rights <$> many tagLineParser


tagLineParser :: Parser (Either () Tag)
tagLineParser =
    AT.eitherP
      (vimTagHeaderLine <?> "failed parsing tag")
      (vimTagParser     <?> "failed parsing header")


vimTagHeaderLine :: Parser ()
vimTagHeaderLine = AT.choice
    [ AT.string (Text.pack "!_TAG_FILE_FORMAT")     *> params
    , AT.string (Text.pack "!_TAG_FILE_SORTED")     *> params
    , AT.string (Text.pack "!_TAG_FILE_ENCODING")   *> params
    , AT.string (Text.pack "!_TAG_PROGRAM_AUTHOR")  *> params
    , AT.string (Text.pack "!_TAG_PROGRAM_NAME")    *> params
    , AT.string (Text.pack "!_TAG_PROGRAM_URL")     *> params
    , AT.string (Text.pack "!_TAG_PROGRAM_VERSION") *> params
    ]
  where
    params = void $ AT.char '\t' *> AT.skipWhile (/= '\n') *> AT.char '\n'


-- | Parse a vim-style tag file.
--
parseVimTagFile :: Text
                -> IO (Either String [Tag])
parseVimTagFile =
      fmap AT.eitherResult
    . AT.parseWith (pure mempty) vimTagFileParser


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
