{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | 'bytestring''s 'Builder' for a 'Tag'
--
module GhcTags.CTags.Formatter
  ( formatTagsFile
  , formatTag
  , formatHeader
  ) where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Char (isAscii)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Text.Printf (printf)

import           GhcTags.Tag
import           GhcTags.Utils (endOfLine)
import           GhcTags.CTags.Utils


-- | 'ByteString' 'Builder' for a single line.
--
formatTag :: CTag -> Builder
formatTag Tag { tagName, tagFilePath, tagAddr, tagKind, tagFields = TagFields tagFields } =

       (BS.byteString . Text.encodeUtf8 . getTagName $ tagName)
    <> BS.charUtf8 '\t'

    <> (BS.byteString . Text.encodeUtf8 . Text.pack $ tagFilePath)
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


formatHeader :: (String, String) -> Builder
formatHeader (header, arg) = BS.stringUtf8 $ printf ("!_" ++ header ++ "\t%s\t//" ++ endOfLine) arg

-- | 'ByteString' 'Builder' for vim 'Tag' file.
--
formatTagsFile :: [(String, String)] -- ^ list of headers
               -> [CTag]             -- ^ 'CTag's
               -> Builder
formatTagsFile headers tags =
       foldMap formatHeader headers
    <> foldMap formatTag tags
