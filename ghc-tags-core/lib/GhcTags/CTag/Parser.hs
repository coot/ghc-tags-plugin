{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Parser combinators for vim style tags (ctags)
--
module GhcTags.CTag.Parser
  ( parseTagsFile
  , parseTagLine
  -- * parse a ctag
  , parseTag
  -- * parse a pseudo-ctag
  , parseHeader
  ) where

import           Control.Arrow ((***))
import           Control.Applicative (many, (<|>))
import           Data.Attoparsec.Text  (Parser, (<?>))
import qualified Data.Attoparsec.Text  as AT
import           Data.Functor (void, ($>))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           System.FilePath (FilePath)

import           GhcTags.Tag
import qualified GhcTags.Utils as Utils
import           GhcTags.CTag.Header
import           GhcTags.CTag.Utils


-- | Parser for a 'CTag' from a single text line.
--
parseTag :: Parser CTag
parseTag =
      (\tagName tagFilePath tagAddr (tagKind, tagFields)
        -> Tag { tagName
               , tagFilePath
               , tagAddr
               , tagKind
               , tagFields
               , tagDefinition = NoTagDefinition
               })
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


parseField :: Parser TagField
parseField =
         TagField
     <$> AT.takeWhile (\x -> x /= ':' && notTabOrNewLine x)
     <*  AT.char ':'
     <*> AT.takeWhile notTabOrNewLine


-- | A vim-style tag file parser.
--
parseTags :: Parser [Either Header CTag]
parseTags = many parseTagLine


-- | Parse either a header line ot a 'CTag'.
--
parseTagLine :: Parser (Either Header CTag)
parseTagLine =
    AT.eitherP
      (parseHeader <?> "failed parsing tag")
      (parseTag    <?> "failed parsing header")


parseHeader :: Parser Header
parseHeader = do
    e <- AT.string "!_TAG_" $> False
         <|>
         AT.string "!_" $> True
    case e of
      True ->
               flip parsePseudoTagArgs (AT.takeWhile notTabOrNewLine)
             . PseudoTag
         =<< AT.takeWhile (\x -> notTabOrNewLine x && x /= '!')
      False -> do
        headerType <-
              AT.string "FILE_ENCODING"     $> SomeHeaderType FileEncoding
          <|> AT.string "FILE_FORMAT"       $> SomeHeaderType FileFormat
          <|> AT.string "FILE_SORTED"       $> SomeHeaderType FileSorted
          <|> AT.string "OUTPUT_MODE"       $> SomeHeaderType OutputMode
          <|> AT.string "KIND_DESCRIPTION"  $> SomeHeaderType KindDescription
          <|> AT.string "KIND_SEPARATOR"    $> SomeHeaderType KindSeparator
          <|> AT.string "PROGRAM_AUTHOR"    $> SomeHeaderType ProgramAuthor
          <|> AT.string "PROGRAM_NAME"      $> SomeHeaderType ProgramName
          <|> AT.string "PROGRAM_URL"       $> SomeHeaderType ProgramUrl
          <|> AT.string "PROGRAM_VERSION"   $> SomeHeaderType ProgramVersion
          <|> AT.string "EXTRA_DESCRIPTION" $> SomeHeaderType ExtraDescription
          <|> AT.string "FIELD_DESCRIPTION" $> SomeHeaderType FieldDescription
        case headerType of
          SomeHeaderType ht@FileEncoding ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@FileFormat ->
              parsePseudoTagArgs ht AT.decimal
          SomeHeaderType ht@FileSorted ->
              parsePseudoTagArgs ht AT.decimal
          SomeHeaderType ht@OutputMode ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@KindDescription ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@KindSeparator ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@ProgramAuthor ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@ProgramName ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@ProgramUrl ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@ProgramVersion ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@ExtraDescription ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType ht@FieldDescription ->
              parsePseudoTagArgs ht (AT.takeWhile notTabOrNewLine)
          SomeHeaderType PseudoTag {} ->
              error "parseHeader: impossible happened"

  where
    parsePseudoTagArgs :: Show ty
                       => HeaderType ty
                       -> Parser ty
                       -> Parser Header
    parsePseudoTagArgs ht parseArg =
              Header ht
          <$> ( (Just <$> (AT.char '!' *> AT.takeWhile notTabOrNewLine))
                <|> pure Nothing
              )
          <*> (AT.char '\t' *> parseArg)
          <*> (AT.char '\t' *> parseComment)

    parseComment :: Parser Text
    parseComment =
         AT.char '/'
      *> (Text.init <$> AT.takeWhile notNewLine)
      <* endOfLine



-- | Parse a vim-style tag file.
--
parseTagsFile :: Text
              -> IO (Either String [Either Header CTag])
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
notTabOrNewLine = \x -> x /= '\t' && notNewLine x

notNewLine :: Char -> Bool
notNewLine = \x -> x /= '\n' && x /= '\r'
