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
module GhcTags.ETags.Parser where

import           Control.Applicative (many, (<|>))
import           Data.Attoparsec.Text  (Parser, (<?>))
import qualified Data.Attoparsec.Text  as AT
import           Data.Functor (($>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.IO (FilePath)

import           GhcTags.Tag
import qualified GhcTags.Utils as Utils


-- | Parse whole etags file
--
parseTagsFile :: Text
              -> IO (Either String [ETag])
parseTagsFile =
      fmap AT.eitherResult
    . AT.parseWith (pure mempty)
                   (concat <$> many parseTagFileSection)


-- | Parse tags from a single file (a sections of etags file).
--
parseTagFileSection :: Parser [ETag]
parseTagFileSection = do
      tagFile <-
        AT.char '\x0c' *> endOfLine
                       *> parseTagFile
      many (parseTag tagFile)

parseTagFile :: Parser FilePath
parseTagFile =
      Text.unpack
  <$> AT.takeWhile (\x -> x /= ',' && Utils.notNewLine x)
  <*  AT.char ','
  <*  (AT.decimal :: Parser Int)
  <*  endOfLine
  <?> "parsing tag file name failed"


parseTag :: FilePath -> Parser (ETag)
parseTag tagFilePath =
          mkTag
      <$> parseTagDefinition
      <*> ((Just <$> parseTagName) <|> pure Nothing)
      <*> AT.decimal
      <*  AT.char ','
      <*> AT.decimal
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
    parseTagName = TagName <$> AT.takeWhile (\x -> x /= '\SOH' && Utils.notNewLine x)
                           <*  AT.char '\SOH'
                           <?> "parsing tag name failed"

    parseTagDefinition :: Parser Text
    parseTagDefinition = AT.takeWhile (\x -> x /= '\DEL' && Utils.notNewLine x)
                     <*  AT.char '\DEL'
                     <?> "parsing tag definition failed"

endOfLine :: Parser ()
endOfLine = AT.string "\r\n" $> ()
        <|> AT.char '\r' $> ()
        <|> AT.char '\n' $> ()
