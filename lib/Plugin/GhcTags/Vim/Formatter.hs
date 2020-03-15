{-# LANGUAGE NamedFieldPuns  #-}

-- | 'bytestring''s 'Builder' for a 'Tag'
--
module Plugin.GhcTags.Vim.Formatter
  ( formatTagsFile
  , formatTag
  , formatHeaders
  , formatHeader
  ) where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Char (isAscii)
import           Data.Version (showVersion)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Text.Printf (printf)

import           Paths_ghc_tags_plugin (version)

import           Plugin.GhcTags.Generate
import           Plugin.GhcTags.Tag


-- | 'ByteString' 'Builder' for a single line.
--
formatTag :: Tag -> Builder
formatTag Tag { tagName, tagFile, tagAddr, tagKind, tagFields} =
       (BS.byteString . Text.encodeUtf8 . getTagName $ tagName)
    <> BS.charUtf8 '\t'
    <> (BS.byteString . Text.encodeUtf8 . Text.pack . getTagFile $ tagFile)
    <> BS.charUtf8 '\t'
    <> formatTagAddress tagAddr
    -- we are using extended format: '_TAG_FILE_FROMAT	2'
    <> BS.stringUtf8 ";\""
    -- tag kind: we are encoding them using field syntax: this is because vim
    -- is using them in the right way: https://github.com/vim/vim/issues/5724
    <> formatKindChar tagKind
    -- tag fields
    <> foldMap ((BS.charUtf8 '\t' <>) . formatField) tagFields 
    <> BS.charUtf8 '\n'
  where

    formatTagAddress :: TagAddress -> Builder
    formatTagAddress (TagLineCol lineNo _colNo) =
      BS.intDec lineNo -- Vim only allows to use ranges; there's no way to
                       -- specify column (`c|` command is not allowed)
    formatTagAddress (TagLine lineNo) =
      BS.intDec lineNo
    formatTagAddress (TagCommand exCommand) =
      BS.byteString . Text.encodeUtf8 . getExCommand $ exCommand     

    formatKindChar :: TagKind -> Builder
    formatKindChar NoKind = mempty
    formatKindChar (CharKind c)
                     | isAscii c = BS.charUtf8 '\t' <> BS.charUtf8 c
                     | otherwise = BS.stringUtf8 "\tkind:" <> BS.charUtf8 c
    formatKindChar (GhcKind ghcKind)
                     | isAscii c = BS.charUtf8 '\t' <> BS.charUtf8 c
                     | otherwise = BS.stringUtf8 "\tkind:" <> BS.charUtf8 c
      where
        c = ghcKindToChar ghcKind


formatField :: TagField -> Builder
formatField TagField { fieldName, fieldValue } =
      BS.byteString (Text.encodeUtf8 fieldName)
   <> BS.charUtf8 ':'
   <> BS.byteString (Text.encodeUtf8 fieldValue)


formatHeader :: String -> String -> String
formatHeader header arg = printf ("!_" ++ header ++ "\t%s\t\n") arg

formatHeaders :: Builder
formatHeaders =
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

-- | 'ByteString' 'Builder' for vim 'Tag' file.
--
formatTagsFile :: [Tag] -> Builder
formatTagsFile tags =
       formatHeaders
    <> foldMap formatTag tags
