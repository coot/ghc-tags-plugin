{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Parser combinators for vim style tags (ctags)
--
module Plugin.GhcTags.CTags.Parser
  ( parseTagsFile
  , parseTagLine
  , parseTags
  , parseTag
  , parseField
  ) where

import           Control.Arrow ((***))
import           Control.Applicative (many, (<|>))
import           Data.Attoparsec.Text  (Parser, (<?>))
import qualified Data.Attoparsec.Text  as AT
import           Data.Functor (void, ($>))
import           Data.Maybe (catMaybes)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           System.IO (FilePath)

import           Plugin.GhcTags.Tag
import qualified Plugin.GhcTags.Utils as Utils


-- | Parser for a single line of a vim-style tag file.
--
parseTag :: Parser CTag
parseTag =
      (\tagName tagFilePath tagAddr (tagKind, tagFields)
        -> Tag { tagName, tagFilePath, tagAddr, tagKind
               , tagFields, tagDefinition = NoTagDefinition })
    <$> parseTagName
    <*  separator

    <*> parseFileName
    <*  separator

    -- includes an optional ';"' separator
    <*> parseTagAddress

    <*> (  -- kind field followed by list of fields or end of line, e.g.
           -- '(TagField, CTagFields)'.
              ((,) <$> ( separator *> parseKindField )
                   <*> ( separator *> parseFields <* endOfLine
                         <|>
                         endOfLine $> mempty)
                       )

          -- list of fields (kind field might be later, but don't check it, we
          -- always format it as the first field) or end of line.
          <|> curry id NoKind
                <$> ( separator *> parseFields <* endOfLine
                      <|>
                      endOfLine $> mempty
                    )

          -- kind encoded as a single letter, followed by a list
          -- of fields or end of line.
          <|> curry (charToTagKind *** id)
                  <$> ( separator *> AT.satisfy notTabOrNewLine )
                  <*> ( separator *> parseFields <* endOfLine
                        <|>
                        endOfLine $> mempty
                      )
          <|> endOfLine $> (NoKind, mempty)
        )

  where
    separator :: Parser Char
    separator = AT.char '\t'

    parseTagName :: Parser TagName
    parseTagName = TagName <$> AT.takeWhile (/= '\t')
                           <?> "parsing tag name failed"

    parseFileName :: Parser FilePath
    parseFileName = Text.unpack <$> AT.takeWhile (/= '\t')

    parseExCommand :: Parser ExCommand
    parseExCommand = (\x -> ExCommand $ Text.take (Text.length x - 1) x)
                 <$> AT.scan "" go
                 <*  AT.anyChar
      where
        -- go until either eol or ';"' sequence is found.
        go :: String -> Char -> Maybe String

        go !s c  | -- eol
                   take (length Utils.endOfLine) (c : s)
                     == reverse Utils.endOfLine
                              = Nothing

                 | -- ';"' sequence
                   l == "\";" = Nothing

                 | otherwise  = Just l
          where
            l = take 2 (c : s)

    -- We only parse `TagLine` or `TagCommand`.
    parseTagAddress :: Parser CTagAddress
    parseTagAddress =
          TagLine <$> AT.decimal <* (endOfLine <|> void (AT.string ";\""))
      <|>
          TagCommand <$> parseExCommand

    parseKindField :: Parser CTagKind
    parseKindField =
      charToTagKind <$>
        (AT.string "kind:" *> AT.satisfy notTabOrNewLine)

    parseFields :: Parser CTagFields
    parseFields = TagFields <$> AT.sepBy parseField separator


charToTagKind :: Char -> CTagKind
charToTagKind c = case charToGhcKind c of
    Nothing      -> CharKind c
    Just ghcTag  -> GhcKind  ghcTag


parseField :: Parser TagField
parseField =
         TagField
     <$> AT.takeWhile (\x -> x /= ':' && notTabOrNewLine x)
     <*  AT.char ':'
     <*> AT.takeWhile notTabOrNewLine


-- | A vim-style tag file parser.
--
parseTags :: Parser [CTag]
parseTags = catMaybes <$> many parseTagLine


parseTagLine :: Parser (Maybe CTag)
parseTagLine =
    either (const Nothing) Just
      <$> AT.eitherP
            (parseHeader <?> "failed parsing tag")
            (parseTag    <?> "failed parsing header")


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
    params = void $ AT.char '\t' *> AT.skipWhile Utils.notNewLine *> endOfLine


-- | Parse a vim-style tag file.
--
parseTagsFile :: Text
              -> IO (Either String [CTag])
parseTagsFile =
      fmap AT.eitherResult
    . AT.parseWith (pure mempty) parseTags


--
-- Utils
--


-- | Unlike 'AT.endOfLine', it also matches for a single '\r' characters (which
-- marks enf of lines on darwin).
--
endOfLine :: Parser ()
endOfLine = AT.string "\r\n" $> ()
        <|> AT.char '\r' $> ()
        <|> AT.char '\n' $> ()


notTabOrNewLine :: Char -> Bool
notTabOrNewLine = \x -> x /= '\t' && x /= '\n' && x /= '\r'
