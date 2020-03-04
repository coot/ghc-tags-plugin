{-# LANGUAGE NamedFieldPuns  #-}

module Plugin.GhcTags.Vim
  ( formatVimTag
  , formatVimTagFile
  ) where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Version (showVersion)
import           Text.Printf (printf)

import           Paths_ghc_tags_plugin (version)

import           Plugin.GhcTags.Generate
import           Plugin.GhcTags.Parser


-- | 'ByteString' 'Builder' for a single line.
--
formatVimTag :: Tag -> Builder
formatVimTag Tag { tagName, tagFile, tagAddr, tagKind, tagFields} =
       BS.byteString (getTagName tagName)
    <> BS.charUtf8 '\t'
    <> BS.byteString (getTagFile tagFile)
    <> BS.charUtf8 '\t'
    <> either BS.intDec BS.byteString tagAddr
    -- we are using extended format: '_TAG_FILE_FROMAT	2'
    <> BS.stringUtf8 ";\""
    -- tag kind
    <> foldMap ((BS.charUtf8 '\t' <>) . BS.charUtf8 . tagKindToChar) tagKind
    -- tag fields
    <> foldMap ((BS.charUtf8 '\t' <>) . formatTagField) tagFields 
    <> BS.charUtf8 '\n'


formatTagField :: TagField -> Builder
formatTagField TagField { fieldName, fieldValue } =
      BS.byteString fieldName
   <> BS.charUtf8 ':'
   <> foldMap BS.byteString fieldValue


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
    <> BS.stringUtf8 (formatHeader "TAG_PROGRAM_VERSION" (showVersion version))
    <> foldMap formatVimTag tags
  where
    formatHeader :: String -> String -> String
    formatHeader header arg = printf ("!_" ++ header ++ "\t%s\t\n") arg
