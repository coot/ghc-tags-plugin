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
  , parseTagsFileRecovering
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
import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           GhcTags.Tag
import qualified GhcTags.Utils as Utils


-- | Parse whole etags file
--
parseTagsFile :: ByteString
              -> IO (Either String [ETag])
parseTagsFile =
      fmap AB.eitherResult
    . AB.parseWith (pure mempty)
                   ((concat . map snd <$> many parseTagFileSection) <* AB.endOfInput)

-- | Parse whole etags file
--
parseTagsFileMap :: ByteString
                 -> IO (Either String ETagMap)
parseTagsFileMap =
      fmap AB.eitherResult
    . AB.parseWith (pure mempty)
                   ((Map.fromList <$> many parseTagFileSection) <* AB.endOfInput)


data Section = OutsideSection | FileHeader | InSection TagFilePath

sectionMarker :: Char
sectionMarker = '\f'

-- | Recover valid tags within and after damaged sections. If a section header
-- is damaged, skip its tags until a new section supplies a trustworthy filename.
parseTagsFileRecovering :: ByteString
                        -> IO (Either String ([String], [ETag]))
parseTagsFileRecovering =
      fmap (fmap partitionEithers . AB.eitherResult)
    . AB.parseWith (pure mempty) (go OutsideSection <* AB.endOfInput)
  where
    go :: Section -> Parser [Either String ETag]
    go section = do
      done <- AB.atEnd
      if done
        then pure $ case section of
          FileHeader -> [Left "skipped incomplete etags section header"]
          _ -> []
        else do
          next <- AChar.peekChar'
          if next == sectionMarker
            then do
              marker <- Utils.parseLine (AChar.char sectionMarker *> endOfLine)
              case marker of
                Left err -> (Left err :) <$> go OutsideSection
                Right () -> case section of
                  FileHeader -> (Left "skipped incomplete etags section header" :) <$> go FileHeader
                  _ -> go FileHeader
            else case section of
              FileHeader -> do
                header <- Utils.parseLine parseTagFilePath
                case header of
                  Left err -> (Left err :) <$> go OutsideSection
                  Right path -> go (InSection path)
              InSection path -> do
                tag <- Utils.parseLine (parseTag path)
                (tag :) <$> go section
              OutsideSection -> do
                line <- Utils.parseLine (fail "tag outside an etags section")
                (line :) <$> go OutsideSection

-- | Parse tags from a single file (a single section in etags file).
--
parseTagFileSection :: Parser (TagFilePath, [ETag])
parseTagFileSection = do
      tagFilePath <-
        AChar.char sectionMarker
          *> endOfLine
          *> parseTagFilePath
      (tagFilePath,) <$> many (parseTag tagFilePath)

parseTagFilePath :: Parser TagFilePath
parseTagFilePath =
      TagFilePath . Text.decodeUtf8
                  . rawFilePathToBS
                  . normaliseRawFilePath
                  . rawFilePathFromBS
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
