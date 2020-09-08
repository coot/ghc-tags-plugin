{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Parser combinators for etags file format
--
module GhcTags.ETag.Parser
  ( parseTagsFile
  , parseTagFileSection
  , parseTag
  ) where

import           Control.Applicative (many, (<|>))
import           Data.ByteString (ByteString)
import           Data.Attoparsec.ByteString  (Parser, (<?>))
import qualified Data.Attoparsec.ByteString  as AB
import qualified Data.Attoparsec.ByteString.Char8  as AChar
import           Data.Functor (($>))
import           Data.Text (Text)
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
                   (concat <$> many parseTagFileSection)


-- | Parse tags from a single file (a single section in etags file).
--
parseTagFileSection :: Parser [ETag]
parseTagFileSection = do
      tagFilePath <-
        AChar.char '\x0c' *> endOfLine
                          *> parseTagFilePath
      many (parseTag tagFilePath)

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
      <*> ((Just <$> parseTagName) <|> pure Nothing)
      <*> AChar.decimal
      <*  AChar.char ','
      <*> AChar.decimal
      <*  endOfLine
      <?> "parsing tag failed"
  where
    mkTag :: Text -> Maybe TagName -> Int -> Int -> ETag
    mkTag tagDefinition mTagName lineNo byteOffset =
      Tag { tagName       = case mTagName of
                              Nothing   -> TagName tagDefinition
                              Just name -> name
          , tagKind       = NoKind
          , tagFilePath
          , tagAddr       = TagLineCol lineNo byteOffset
          , tagDefinition = case mTagName of
                              Nothing -> NoTagDefinition
                              Just _  -> TagDefinition tagDefinition
          , tagFields     = NoTagFields
          }

    parseTagName :: Parser TagName
    parseTagName =
          TagName . Text.decodeUtf8
      <$> AChar.takeWhile (\x -> x /= '\SOH' && Utils.notNewLine x)
      <*  AChar.char '\SOH'
      <?> "parsing tag name failed"

    parseTagDefinition :: Parser Text
    parseTagDefinition =
          Text.decodeUtf8
      <$> AChar.takeWhile (\x -> x /= '\DEL' && Utils.notNewLine x)
      <*  AChar.char '\DEL'
      <?> "parsing tag definition failed"

endOfLine :: Parser ()
endOfLine = AChar.string "\r\n" $> ()
        <|> AChar.char '\r' $> ()
        <|> AChar.char '\n' $> ()
