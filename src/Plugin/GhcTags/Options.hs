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


-- | /ghc-tags-plugin/ options
--
data Options f = Options
  { etags    :: Bool
    -- ^ if 'True' use emacs tags file format, the default is 'False'.

  , filePath :: f FilePath
    -- ^ file path to the tags file (relative to the @*.cabal@ file).  The
    -- default is either 'tags' (if 'etags' if 'False') or 'TAGS' otherwise.
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
    defaultOptions Options { etags, filePath } =
      case filePath of
        Nothing -> Options { etags, filePath = Identity (bool "tags" "TAGS" etags) }
        Just fp -> Options { etags, filePath = Identity fp }
