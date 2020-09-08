{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'bytestring''s 'Builder' for a 'Tag'
--
module GhcTags.CTag.Formatter
  ( formatTagsFile
  -- * format a ctag
  , formatTag
  -- * format a pseudo-ctag
  , formatHeader
  ) where

import           Control.Arrow ((|||))
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Char (isAscii)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as Text

import           GhcTags.Tag
import           GhcTags.Utils (endOfLine)
import           GhcTags.CTag.Header
import           GhcTags.CTag.Utils


-- | 'ByteString' 'Builder' for a single line.
--
formatTag :: CTag -> Builder
formatTag Tag { tagName, tagFilePath, tagAddr, tagKind, tagFields = TagFields tagFields } =

       (BS.byteString . Text.encodeUtf8 . getTagName $ tagName)
    <> BS.charUtf8 '\t'

    <> BS.byteString (Text.encodeUtf8 . getRawFilePath $ tagFilePath)
    <> BS.charUtf8 '\t'

    <> formatTagAddress tagAddr
    -- we are using extended format: '_TAG_FILE_FROMAT	2'
    <> BS.stringUtf8 ";\""

    -- tag kind: we are encoding them using field syntax: this is because vim
    -- is using them in the right way: https://github.com/vim/vim/issues/5724
    <> formatKindChar tagKind

    -- tag fields
    <> foldMap ((BS.charUtf8 '\t' <>) . formatField) tagFields

    <> BS.stringUtf8 endOfLine

  where

    formatTagAddress :: CTagAddress -> Builder
    formatTagAddress (TagLineCol lineNo _colNo) =
      BS.intDec lineNo -- Vim only allows to use ranges; there's no way to
                       -- specify column (`c|` command is not allowed)
    formatTagAddress (TagLine lineNo) =
      BS.intDec lineNo
    formatTagAddress (TagCommand exCommand) =
      BS.byteString . Text.encodeUtf8 . getExCommand $ exCommand

    formatKindChar :: CTagKind -> Builder
    formatKindChar tk =
      case tagKindToChar tk of
        Nothing -> mempty
        Just c | isAscii c -> BS.charUtf8 '\t' <> BS.charUtf8 c
               | otherwise -> BS.stringUtf8 "\tkind:" <> BS.charUtf8 c


formatField :: TagField -> Builder
formatField TagField { fieldName, fieldValue } =
      BS.byteString (Text.encodeUtf8 fieldName)
   <> BS.charUtf8 ':'
   <> BS.byteString (Text.encodeUtf8 fieldValue)


formatHeader :: Header -> Builder
formatHeader Header { headerType, headerLanguage, headerArg, headerComment } =
    case headerType of
      FileEncoding ->
        formatTextHeaderArgs "FILE_ENCODING"     headerLanguage headerArg headerComment
      FileFormat ->
        formatIntHeaderArgs "FILE_FORMAT"        headerLanguage headerArg headerComment
      FileSorted ->
        formatIntHeaderArgs "FILE_SORTED"        headerLanguage headerArg headerComment
      OutputMode ->
        formatTextHeaderArgs "OUTPUT_MODE"       headerLanguage headerArg headerComment
      KindDescription ->
        formatTextHeaderArgs "KIND_DESCRIPTION"  headerLanguage headerArg headerComment
      KindSeparator ->
        formatTextHeaderArgs "KIND_SEPARATOR"    headerLanguage headerArg headerComment
      ProgramAuthor ->
        formatTextHeaderArgs "PROGRAM_AUTHOR"    headerLanguage headerArg headerComment
      ProgramName ->
        formatTextHeaderArgs "PROGRAM_NAME"      headerLanguage headerArg headerComment
      ProgramUrl ->
        formatTextHeaderArgs "PROGRAM_URL"       headerLanguage headerArg headerComment
      ProgramVersion ->
        formatTextHeaderArgs "PROGRAM_VERSION"   headerLanguage headerArg headerComment
      ExtraDescription ->
        formatTextHeaderArgs "EXTRA_DESCRIPTION" headerLanguage headerArg headerComment
      FieldDescription ->
        formatTextHeaderArgs "FIELD_DESCRIPTION" headerLanguage headerArg headerComment
      PseudoTag name ->
        formatHeaderArgs (BS.byteString . Text.encodeUtf8)
                         "!_" name headerLanguage headerArg headerComment
  where
    formatHeaderArgs :: (ty -> Builder)
                     -> String
                     -> Text
                     -> Maybe Text
                     -> ty
                     -> Text
                     -> Builder
    formatHeaderArgs formatArg prefix headerName language arg comment =
         BS.stringUtf8 prefix
      <> BS.byteString (Text.encodeUtf8 headerName)
      <> foldMap ((BS.charUtf8 '!' <>) . BS.byteString . Text.encodeUtf8) language
      <> BS.charUtf8 '\t'
      <> formatArg arg
      <> BS.stringUtf8 "\t/"
      <> BS.byteString (Text.encodeUtf8 comment)
      <> BS.charUtf8 '/'
      <> BS.stringUtf8 endOfLine

    formatTextHeaderArgs = formatHeaderArgs (BS.byteString . Text.encodeUtf8) "!_TAG_"
    formatIntHeaderArgs  = formatHeaderArgs BS.intDec "!_TAG_"


-- | 'ByteString' 'Builder' for vim 'Tag' file.
--
formatTagsFile :: [Either Header CTag]             -- ^ 'CTag's
               -> Builder
formatTagsFile tags =
    foldMap (formatHeader ||| formatTag) tags
