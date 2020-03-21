{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plugin.GhcTags.Options
  ( Options (..)
  , ParserResult (..)
  , runOptionParser
  ) where

import           Data.Bool (bool)
import           Data.Functor.Identity (Identity (..))
import           System.IO (FilePath)
import           Options.Applicative

etagsParser :: Parser Bool
etagsParser = switch $
       short 'e'
    <> long "etags"
    <> showDefault
    <> help "produce emacs etags file"

filePathParser :: Parser (Maybe FilePath)
filePathParser =
          (Just <$> strArgument
                      (help "tags file: default tags or TAGS (when --etags is specified)"
                       <> metavar "file_path"
                      ))
      <|> pure Nothing


data Options f = Options
  { etags    :: Bool
  , filepath :: f FilePath
  }

deriving instance Show (Options Maybe)
deriving instance Show (Options Identity)

parseOtions :: Parser (Options Maybe)
parseOtions = Options
           <$> etagsParser
           <*> filePathParser


parserInfo :: ParserInfo (Options Maybe)
parserInfo = info (parseOtions <**> helper) $
       progDesc "write tags from ghc abstract syntax tree"
    <> fullDesc


runOptionParser :: [String]
                -> ParserResult (Options Identity)
runOptionParser = fmap defaultOptions . execParserPure defaultPrefs parserInfo
  where
    defaultOptions :: Options Maybe -> Options Identity
    defaultOptions Options { etags, filepath } =
      case filepath of
        Nothing -> Options { etags, filepath = Identity (bool "tags" "TAGS" etags) }
        Just fp -> Options { etags, filepath = Identity fp }
