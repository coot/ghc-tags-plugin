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
import           Data.Attoparsec.Text  (Parser, (<?>))
import qualified Data.Attoparsec.Text  as AT
import           Data.Functor (($>))
import           Data.Text (Text)

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


-- | Parse tags from a single file (a single section in etags file).
--
parseTagFileSection :: Parser [ETag]
parseTagFileSection = do
      tagFilePath <-
        AT.char '\x0c' *> endOfLine
                       *> parseTagFilePath
      many (parseTag tagFilePath)

parseTagFilePath :: AT.Parser TagFilePath
parseTagFilePath =
      TagFilePath
  <$> AT.takeWhile (\x -> x /= ',' && Utils.notNewLine x)
  <*  AT.char ','
  <*  (AT.decimal :: Parser Int)
  <*  endOfLine
  <?> "parsing tag file name failed"


-- | Parse an 'ETag' from a single line.
--
parseTag :: TagFilePath -> Parser ETag
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
