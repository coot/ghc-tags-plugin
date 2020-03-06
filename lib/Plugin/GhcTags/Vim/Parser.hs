{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

-- | Parser combinators for vim style tags
--
module Plugin.GhcTags.Vim.Parser
  ( parseTagsFile
  , parseTags
  , parseTag
  , parseField
  ) where

import           Control.Applicative (many, (<|>))
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
        (\n f l (k, fs) -> Tag n f l k fs)
    <$> parseName
    <*  separator

    <*> parseFile
    <*  separator

    -- includes an optional ';"' separator
    <*> AT.eitherP parseAddr parseExCommand

    <*> (  -- kind field followed by list of fields
              separator *>
              ((,) <$> parseKindField
                   <*> parseFields)
          -- list of fields (kind field might be later, but we'll never check for it)
          <|> ((Nothing,) <$> parseFields)
          -- kind encoded as a single letter, followed by a list of fields
          <|> separator *>
              ((,) <$> (charToTagKind <$> AT.satisfy notTabOrNewLine)
                   <*> parseFields)
          <|> AT.char '\n' *> pure (Nothing, [])
        )

  where
    separator = AT.char '\t'
    notTabOrNewLine = \x -> x /= '\t' && x /= '\n' 

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

    parseKindField :: Parser (Maybe TagKind)
    parseKindField =
      charToTagKind <$>
        (AT.string "kind:" *> AT.satisfy notTabOrNewLine)

    parseFields :: Parser [TagField]
    parseFields =
        (either (const []) id
          <$> AT.eitherP 
                AT.endOfLine
                (separator
                  *> AT.sepBy parseField separator
                   <* AT.endOfLine))


parseField :: Parser TagField
parseField =
         TagField
     <$> AT.takeWhile (\x -> x /= ':' && x /= '\n' && x /= '\t')
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
