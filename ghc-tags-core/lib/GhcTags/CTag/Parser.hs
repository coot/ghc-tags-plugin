{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser combinators for vim style tags (ctags)
--
module GhcTags.CTag.Parser
  ( parseTagsFile
  , parseTagsFileMap
  , parseTagLine
  -- * parse a ctag
  , parseTag
  -- * parse a pseudo-ctag
  , parseHeader
  ) where

import           Control.Arrow ((***))
import           Control.Applicative (many, (<|>))
import           Control.DeepSeq (NFData)
import           Control.Monad (guard)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString  (Parser, (<?>))
import qualified Data.Attoparsec.ByteString  as AB
import qualified Data.Attoparsec.ByteString.Char8  as AChar
import           Data.Either (partitionEithers)
import           Data.Functor (void, ($>))
import           Data.Function (on)
import qualified Data.Map.Strict as Map
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified System.FilePath.ByteString as FilePath

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

    <*> parseTagFileName
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
                  <$> ( separator *> AChar.satisfy notTabOrNewLine )
                  <*> ( separator *> parseFields <* endOfLine
                        <|>
                        endOfLine $> mempty
                      )
          <|> endOfLine $> (NoKind, mempty)
        )

  where
    separator :: Parser Char
    separator = AChar.char '\t'

    parseTagName :: Parser TagName
    parseTagName = TagName . Text.decodeUtf8
                    <$> AChar.takeWhile (/= '\t')
                    <?> "parsing tag name failed"

    parseTagFileName :: Parser TagFilePath
    parseTagFileName =
          TagFilePath . Text.decodeUtf8 . FilePath.normalise
      <$> AChar.takeWhile (/= '\t')

    parseExCommand :: Parser ExCommand
    parseExCommand = (\x -> ExCommand $ Text.decodeUtf8 $ BS.take (BS.length x - 1) x)
                 <$> AChar.scan "" go
                 <*  AChar.anyChar
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
          TagLine <$> AChar.decimal <* (endOfLine <|> void (AB.string ";\""))
      <|>
          TagCommand <$> parseExCommand

    parseKindField :: Parser TagKind
    parseKindField = do
      x <-
        Text.decodeUtf8
          <$> (AB.string "kind:" *> AChar.takeWhile notTabOrNewLine)
      guard (Text.length x == 1)
      pure $ charToTagKind (Text.head x)

    parseFields :: Parser CTagFields
    parseFields = TagFields <$> AChar.sepBy parseField separator


parseField :: Parser TagField
parseField =
         on TagField Text.decodeUtf8
     <$> AChar.takeWhile (\x -> x /= ':' && notTabOrNewLine x)
     <*  AChar.char ':'
     <*> AChar.takeWhile notTabOrNewLine


-- | A vim-style tag file parser.
--
parseTags :: Parser [Either Header CTag]
parseTags = many parseTagLine


-- | Parse either a header line ot a 'CTag'.
--
parseTagLine :: Parser (Either Header CTag)
parseTagLine =
    AChar.eitherP
      (parseHeader <?> "failed parsing tag")
      (parseTag    <?> "failed parsing header")


parseHeader :: Parser Header
parseHeader = do
    e <- AB.string "!_TAG_" $> False
         <|>
         AB.string "!_" $> True
    if e then flip parsePseudoTagArgs (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
            . PseudoTag
            . Text.decodeUtf8
        =<< AChar.takeWhile (\x -> notTabOrNewLine x && x /= '!')
         else do
      headerType <-
            AB.string "FILE_ENCODING"     $> SomeHeaderType FileEncoding
        <|> AB.string "FILE_FORMAT"       $> SomeHeaderType FileFormat
        <|> AB.string "FILE_SORTED"       $> SomeHeaderType FileSorted
        <|> AB.string "OUTPUT_MODE"       $> SomeHeaderType OutputMode
        <|> AB.string "KIND_DESCRIPTION"  $> SomeHeaderType KindDescription
        <|> AB.string "KIND_SEPARATOR"    $> SomeHeaderType KindSeparator
        <|> AB.string "PROGRAM_AUTHOR"    $> SomeHeaderType ProgramAuthor
        <|> AB.string "PROGRAM_NAME"      $> SomeHeaderType ProgramName
        <|> AB.string "PROGRAM_URL"       $> SomeHeaderType ProgramUrl
        <|> AB.string "PROGRAM_VERSION"   $> SomeHeaderType ProgramVersion
        <|> AB.string "EXTRA_DESCRIPTION" $> SomeHeaderType ExtraDescription
        <|> AB.string "FIELD_DESCRIPTION" $> SomeHeaderType FieldDescription
      case headerType of
        SomeHeaderType ht@FileEncoding ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@FileFormat ->
            parsePseudoTagArgs ht AChar.decimal
        SomeHeaderType ht@FileSorted ->
            parsePseudoTagArgs ht AChar.decimal
        SomeHeaderType ht@OutputMode ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@KindDescription ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@KindSeparator ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@ProgramAuthor ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@ProgramName ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@ProgramUrl ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@ProgramVersion ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@ExtraDescription ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType ht@FieldDescription ->
            parsePseudoTagArgs ht (Text.decodeUtf8 <$> AChar.takeWhile notTabOrNewLine)
        SomeHeaderType PseudoTag {} ->
            error "parseHeader: impossible happened"

  where
    parsePseudoTagArgs :: NFData ty
                       => Show ty
                       => HeaderType ty
                       -> Parser ty
                       -> Parser Header
    parsePseudoTagArgs ht parseArg =
              Header ht
          <$> ( (Just . Text.decodeUtf8 <$> (AChar.char '!' *> AChar.takeWhile notTabOrNewLine))
                <|> pure Nothing
              )
          <*> (AChar.char '\t' *> parseArg)
          <*> (AChar.char '\t' *> parseComment)

    parseComment :: Parser Text
    parseComment =
         AChar.char '/'
      *> (Text.init . Text.decodeUtf8 <$> AChar.takeWhile notNewLine)
      <* endOfLine



-- | Parse a vim-style tag file.
--
parseTagsFile :: ByteString
              -> IO (Either String [Either Header CTag])
parseTagsFile =
      fmap AChar.eitherResult
    . AChar.parseWith (pure mempty) parseTags



-- | Parse a vim-style tag file.
--
parseTagsFileMap :: ByteString
                 -> IO (Either String ([Header], CTagMap))
parseTagsFileMap =
    fmap (fmap f) . parseTagsFile
  where
    f :: [Either Header CTag] -> ([Header], CTagMap)
    f as = case partitionEithers as of
      (headers, tags) ->
        (headers, Map.fromListWith (++) [(tagFilePath tag, [tag]) | tag <- tags])

--
-- Utils
--


-- | Unlike 'AChar.endOfLine', it also matches for a single '\r' characters (which
-- marks enf of lines on darwin).
--
endOfLine :: Parser ()
endOfLine = AB.string "\r\n" $> ()
        <|> AChar.char '\r' $> ()
        <|> AChar.char '\n' $> ()


notTabOrNewLine :: Char -> Bool
notTabOrNewLine = \x -> x /= '\t' && notNewLine x

notNewLine :: Char -> Bool
notNewLine = \x -> x /= '\n' && x /= '\r'
