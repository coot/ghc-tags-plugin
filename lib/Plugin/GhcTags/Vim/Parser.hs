{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Parser combinators for vim style tags
--
module Plugin.GhcTags.Vim.Parser
  ( parseTagsFile
  , parseTags
  , parseTag
  ) where

import           Control.Applicative (many)
import           Data.Attoparsec.Text  (Parser, (<?>))
import qualified Data.Attoparsec.Text  as AT
import           Data.Either (rights)
import           Data.Functor (void)
import           Data.Text          (Text)
import qualified Data.Text          as Text

import           Plugin.GhcTags.Tag


-- | Parser for a single line of a vim-style tag file.
--
parseTag :: Parser Tag
parseTag =
        Tag
    <$> parseName
    <*  separator

    <*> parseFile
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

    parseName :: Parser TagName
    parseName = TagName <$> AT.takeWhile (/= '\t')
                           <?> "parsing tag name failed"

    parseFile :: Parser TagFile
    parseFile = TagFile <$> AT.takeWhile (/= '\t')

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
parseTags :: Parser [Tag]
parseTags = rights <$> many parseTagLine


parseTagLine :: Parser (Either () Tag)
parseTagLine =
    AT.eitherP
      (parseHeader <?> "failed parsing tag")
      (parseTag     <?> "failed parsing header")


parseHeader :: Parser ()
parseHeader = AT.choice
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
parseTagsFile :: Text
              -> IO (Either String [Tag])
parseTagsFile =
      fmap AT.eitherResult
    . AT.parseWith (pure mempty) parseTags
