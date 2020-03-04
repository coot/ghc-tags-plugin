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
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
  { tagName   :: !TagName
  , tagFile   :: !TagFile
  , tagAddr   :: !(Either Int ByteString)
  , tagKind   :: !(Maybe TagKind)
  , tagFields :: ![TagField]
  }
  deriving (Ord, Eq, Show)


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { gtSrcSpan, gtTag, gtKind, gtExported } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName   = TagName (fs_bs gtTag)
                   , tagFile   = TagFile (fs_bs (srcSpanFile realSrcSpan))
                   , tagAddr   = Left (srcSpanStartLine realSrcSpan)
                   , tagKind   = Just gtKind
                   , tagFields = if gtExported then [] else [fileField]
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

    <*> AC.eitherP parseAddr parseExCommand
    <* separator

    <*> (charToTagKind <$> AC.anyChar)
    <*> many (separator *> parseField)

    <*  AC.endOfLine
  where
    separator = AC.char '\t'

    parseTagName :: Parser TagName
    parseTagName = TagName <$> AC.takeWhile (/= '\t')
                           <?> "parsing tag name failed"

    parseTagFile :: Parser TagFile
    parseTagFile = TagFile <$> AC.takeWhile (/= '\t')

    parseExCommand :: Parser ByteString
    parseExCommand = (\x -> BS.take (BS.length x - 1) x) <$>
                     AC.scan "" go
                  <* AC.anyChar
      where
        -- go until either '`n' or ';"' sequence is found.
        go :: String -> Char -> Maybe String
        go _ '\n'             = Nothing
        go !s c  | l == "\";" = Nothing
                 | otherwise  = Just l
          where
            l = take 2 (c : s)

    parseAddr :: Parser Int
    parseAddr = AC.decimal
             <* AC.eitherP
                  AC.endOfLine
                  (AC.char ';' *> AC.char '"')


data TagField = TagField {
      fieldName  :: ByteString,
      fieldValue :: Maybe ByteString
    }
  deriving (Eq, Ord, Show)

fileField :: TagField
fileField = TagField { fieldName = "file", fieldValue = Nothing }

parseField :: Parser TagField
parseField =
         TagField
     <$> AC.takeWhile (\x -> x /= ':'  && x /= '\n')
     <*  AC.char ':'
     <*> (toValue <$> AC.takeWhile (\x -> x /= '\t' && x /= '\n'))
  where
    toValue :: ByteString -> Maybe ByteString
    toValue "" = Nothing
    toValue bs = Just bs



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
