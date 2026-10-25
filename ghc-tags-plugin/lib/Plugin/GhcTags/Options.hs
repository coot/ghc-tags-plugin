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
import           Options.Applicative


etagsParser :: Parser Bool
etagsParser = switch $
       short 'e'
    <> long "etags"
    <> showDefault
    <> help "produce emacs etags file"

streamParser :: Parser Bool
streamParser = switch $
       short 's'
    <> long "stream"
    <> showDefault
    <> help ( "stream tags from the tags file when updating its contents"
           ++ " with the tags found in the current module" )

filePathParser :: Parser FilePath
filePathParser =
    strArgument $
         help "tags file relative to cabal.project or stack.yaml: default tags or TAGS (with --etags)"
      <> metavar "file_path"

debugParser :: Parser Bool
debugParser = switch $
       long "debug"
    <> showDefault
    <> help "debug"

qualifiedParser :: Parser Bool
qualifiedParser = switch $
       short 'q'
    <> long "qualified"
    <> help "also generate names qualified by the defining module"

-- | /ghc-tags-plugin/ options
--
data Options f = Options
  { etags    :: Bool
    -- ^ if 'True' use emacs tags file format, the default is 'False'.

  , stream   ::   Bool
    -- ^ be default we read the tags file and overwrite it.  When this option
    -- is on, we stream tags from it while interleaving the tags found in the
    -- current module to a new destination, which is then moved to the tags
    -- file destination.

  , filePath :: f FilePath
    -- ^ file path to the tags file, relative to @cabal.project@ or @stack.yaml@.
    -- If neither is found, use the @*.cabal@ package directory and warn. The
    -- default is either 'tags' (if 'etags' if 'False') or 'TAGS' otherwise.

  , debug :: Bool

  , qualified :: Bool
    -- ^ Also generate module-qualified aliases alongside ordinary names.
  }

deriving instance Show (Options Identity)


parseOtions :: Parser (Options Last)
parseOtions = Options
         <$> etagsParser
         -- allow to pass the argument multiple times
         <*> streamParser
         <*> (foldMap (Last . Just) <$> many filePathParser)
         <*> debugParser
         <*> qualifiedParser


parserInfo :: ParserInfo (Options Last)
parserInfo = info (parseOtions <**> helper) $
       progDesc "write tags from ghc abstract syntax tree"
    <> fullDesc


runOptionParser :: [String]
                -> ParserResult (Options Identity)
runOptionParser = fmap defaultOptions . execParserPure defaultPrefs parserInfo
  where
    defaultOptions :: Options Last -> Options Identity
    defaultOptions Options { etags, stream, filePath, debug, qualified } =
        Options {
            etags,
            stream,
            filePath = Identity filePath',
            debug,
            qualified
          }
      where
        filePath' =
          case filePath of
            Last Nothing   -> bool "tags" "TAGS" etags
            Last (Just fp) -> fp
