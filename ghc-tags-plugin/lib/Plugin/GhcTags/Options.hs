{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plugin.GhcTags.Options
  ( Options (..)
  , ParserResult (..)
  , runOptionParser
  ) where

import           Data.Bool (bool)
import           Data.Monoid (Last (..))
import           Data.Functor.Identity (Identity (..))
import           System.IO (FilePath)
import           Options.Applicative


etagsParser :: Parser Bool
etagsParser = switch $
       short 'e'
    <> long "etags"
    <> showDefault
    <> help "produce emacs etags file"


filePathParser :: Parser (FilePath)
filePathParser =
    strArgument $
         help "tags file: default tags or TAGS (when --etags is specified)"
      <> metavar "file_path"


-- | /ghc-tags-plugin/ options
--
data Options f = Options
  { etags    :: Bool
    -- ^ if 'True' use emacs tags file format, the default is 'False'.

  , filePath :: f FilePath
    -- ^ file path to the tags file (relative to the @*.cabal@ file).  The
    -- default is either 'tags' (if 'etags' if 'False') or 'TAGS' otherwise.
  }

deriving instance Show (Options Identity)


parseOtions :: Parser (Options Last)
parseOtions = Options
         <$> etagsParser
         -- allow to pass the argument multiple times
         <*> (foldMap (Last . Just) <$> many filePathParser)


parserInfo :: ParserInfo (Options Last)
parserInfo = info (parseOtions <**> helper) $
       progDesc "write tags from ghc abstract syntax tree"
    <> fullDesc


runOptionParser :: [String]
                -> ParserResult (Options Identity)
runOptionParser = fmap defaultOptions . execParserPure defaultPrefs parserInfo
  where
    defaultOptions :: Options Last -> Options Identity
    defaultOptions Options { etags, filePath } =
        Options {
            etags,
            filePath = Identity filePath'
          }
      where
        filePath' =
          case filePath of
            Last Nothing   -> bool "tags" "TAGS" etags
            Last (Just fp) -> fp
