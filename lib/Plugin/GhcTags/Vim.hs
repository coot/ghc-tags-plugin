{-# LANGUAGE NamedFieldPuns  #-}
-- only to get hash of the current commit
{-# LANGUAGE TemplateHaskell #-}

module Plugin.GhcTags.Vim
  ( formatVimTag
  , formatVimTagFile
  ) where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Bool (bool)
import           Data.Version (showVersion)
import           Text.Printf (printf)

import qualified Development.GitRev as GitRev

import           Paths_ghc_tags_plugin (version)

import           Plugin.GhcTags.Generate
import           Plugin.GhcTags.Parser


-- | 'ByteString' 'Builder' for a single line.
--
formatVimTag :: Tag -> Builder
formatVimTag Tag { tagName, tagFile, tagLine, tagKind } =
       BS.byteString (getTagName tagName)
    <> BS.charUtf8 '\t'
    <> BS.byteString (getTagFile tagFile)
    <> BS.charUtf8 '\t'
    <> BS.intDec tagLine
    <> case tagKind of
        Just k ->
             BS.stringUtf8 ";\"\t"
          <> BS.charUtf8 (tagKindToChar k)
        Nothing -> mempty
    <> BS.charUtf8 '\n'

-- | 'ByteString' 'Builder' for vim 'Tag' file.
--
formatVimTagFile :: [Tag] -> Builder
formatVimTagFile tags =
       -- format 1 does not append ';"' to lines
       BS.stringUtf8 (formatHeader "TAG_FILE_FORMAT"    "2")
       -- allows for  binary search
    <> BS.stringUtf8 (formatHeader "TAG_FILE_SORTED"    "1")
    <> BS.stringUtf8 (formatHeader "TAG_FILE_ENCODING"  "utf-8")
    <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_AUTHOR" "Marcin Szamotulski")
    <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_NAME"   "ghc-tags-pluginn")
    <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_URL"
                                   "https://hackage.haskell.org/package/ghc-tags-plugin")
       -- version number with git revision
    <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_VERSION"
                                    (showVersion version ++ " (" ++ gitExtra ++ ")"))
    <> foldMap formatVimTag tags
  where
    formatHeader :: String -> String -> String
    formatHeader header arg = printf ("!_" ++ header ++ "\t%s\t\n")  arg

    gitExtra :: String
    gitExtra = $(GitRev.gitHash) ++ bool "" " DIRTY" $(GitRev.gitDirty)
