{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

-- | Parser combinators for etags file format
--
module GhcTags.ETag.Parser
  ( parseTagsFile
  , parseTagsFileMap
  , parseTagFileSection
  , parseTag
  ) where

import           Control.Applicative (many, (<|>))
import           Data.ByteString (ByteString)
import           Data.Attoparsec.ByteString  (Parser, (<?>))
import qualified Data.Attoparsec.ByteString  as AB
import qualified Data.Attoparsec.ByteString.Char8  as AChar
import           Data.Functor (($>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.FilePath.ByteString as FilePath

import           GhcTags.Tag
import qualified GhcTags.Utils as Utils


-- | Parse whole etags file
--
parseTagsFile :: ByteString
              -> IO (Either String [ETag])
parseTagsFile =
      fmap AB.eitherResult
    . AB.parseWith (pure mempty)
                   (concat . map snd <$> many parseTagFileSection)

-- | Parse whole etags file
--
parseTagsFileMap :: ByteString
                 -> IO (Either String ETagMap)
parseTagsFileMap =
      fmap AB.eitherResult
    . AB.parseWith (pure mempty)
                   (Map.fromList <$> many parseTagFileSection)

-- | Parse tags from a single file (a single section in etags file).
--
parseTagFileSection :: Parser (TagFilePath, [ETag])
parseTagFileSection = do
      tagFilePath <-
        AChar.char '\x0c' *> endOfLine
                          *> parseTagFilePath
      (tagFilePath,) <$> many (parseTag tagFilePath)

parseTagFilePath :: Parser TagFilePath
parseTagFilePath =
      TagFilePath . Text.decodeUtf8 . FilePath.normalise
  <$> AChar.takeWhile (\x -> x /= ',' && Utils.notNewLine x)
  <*  AChar.char ','
  <*  (AChar.decimal :: Parser Int)
  <*  endOfLine
  <?> "parsing tag file name failed"


-- | Parse an 'ETag' from a single line.
--
parseTag :: TagFilePath -> Parser ETag
parseTag tagFilePath =
          mkTag
      <$> parseTagDefinition
      <*> parseTagName
      <*> parseAddress
      <?> "parsing tag failed"
  where
    parseAddress :: Parser ETagAddress
    parseAddress =
          TagLine    <$> AChar.decimal
                     <*  AChar.char ','
                     <*  endOfLine
      <|> TagLineCol <$> AChar.decimal
                     <*  AChar.char ','
                     <*> AChar.decimal
                     <*  endOfLine
      <|> NoAddress  <$  AChar.char ','
                     <*  endOfLine

    mkTag :: TagDefinition ETAG -> TagName -> ETagAddress -> ETag
    mkTag tagDefinition tagName tagAddr =
      Tag { tagName       = tagName
          , tagKind       = NoKind
          , tagFilePath
          , tagAddr
          , tagDefinition
          , tagFields     = NoTagFields
          }

    parseTagName :: Parser TagName
    parseTagName =
          TagName . Text.decodeUtf8
      <$> AChar.takeWhile (\x -> x /= '\SOH' && Utils.notNewLine x)
      <*  AChar.char '\SOH'
      <?> "parsing tag name failed"

    parseTagDefinition :: Parser (TagDefinition ETAG)
    parseTagDefinition =
            (\t -> if Text.null t
                     then NoTagDefinition
                     else TagDefinition t)
          . Text.decodeUtf8
      <$> AChar.takeWhile (\x -> x /= '\DEL' && Utils.notNewLine x)
      <*  AChar.char '\DEL'
      <?> "parsing tag definition failed"

endOfLine :: Parser ()
endOfLine = AChar.string "\r\n" $> ()
        <|> AChar.char '\r' $> ()
        <|> AChar.char '\n' $> ()
