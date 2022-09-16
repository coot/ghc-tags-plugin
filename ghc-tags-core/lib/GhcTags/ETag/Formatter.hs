{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Simple etags formatter. See <https://en.wikipedia.org/wiki/Ctags#Etags>
--
module GhcTags.ETag.Formatter
  ( formatETagsFile
  , formatTagsFileMap
  , formatTagsFile
  , formatTag
  , BuilderWithSize (..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import           Data.List (groupBy)
import           Data.Function (on)
-- import           Data.Foldable (foldl')
import qualified Data.Text.Encoding as Text

import           GhcTags.Tag


-- | A product of two monoids: 'Builder' and 'Sum'.
--
data BuilderWithSize = BuilderWithSize {
    builder     :: Builder,
    builderSize :: !Int
  }

instance Semigroup BuilderWithSize where
    BuilderWithSize b0 s0 <> BuilderWithSize b1 s1 =
      BuilderWithSize (b0 <> b1) (s0 + s1)

instance Monoid BuilderWithSize where
    mempty = BuilderWithSize mempty 0

formatTag :: ETag -> BuilderWithSize
formatTag Tag {tagName, tagAddr, tagDefinition} =
           flip BuilderWithSize tagSize $
        -- TODO: get access to the original line or pretty print original
        -- declaration
           BB.byteString tagDefinitionBS
        <> BB.charUtf8 '\DEL' -- or '\x7f'
        <> BB.byteString tagNameBS
        <> BB.charUtf8 '\SOH' -- or '\x01'
        <> BB.byteString tagAddressBS
        <> BB.stringUtf8 endOfLine
  where
    tagNameBS :: BS.ByteString
    tagNameBS = Text.encodeUtf8 . getTagName $ tagName
    tagNameSize = BS.length tagNameBS

    tagDefinitionBS :: BS.ByteString
    tagDefinitionBS = case tagDefinition of
        NoTagDefinition   -> mempty
        TagDefinition def -> Text.encodeUtf8 def
    tagDefinitionSize = BS.length tagDefinitionBS

    tagAddressBS :: BS.ByteString
    tagAddressBS = case tagAddr of
       TagLine lineNo ->
             BS.Char8.pack (show lineNo)
          <> BS.Char8.singleton ','
       TagLineCol lineNo offset ->
             BS.Char8.pack (show lineNo)
          <> BS.Char8.singleton ','
          <> BS.Char8.pack (show offset)
       NoAddress ->
             BS.Char8.singleton ','
    tagAddressSize = BS.length tagAddressBS

    tagSize =
        2 -- delimiters: '\DEL', '\SOH'
      + tagDefinitionSize
      + tagNameSize
      + tagAddressSize
      + length endOfLine


-- | The precondition is that all the tags come frome the same file.
--
formatTagsFile :: [ETag] -> Builder
formatTagsFile [] = mempty
formatTagsFile ts@(Tag {tagFilePath} : _) =
    case foldMap formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        if builderSize > 0
          then BB.charUtf8 '\x0c'
            <> BB.stringUtf8 endOfLine
            <> BB.byteString (Text.encodeUtf8 $ getRawFilePath tagFilePath)
            <> BB.charUtf8 ','
            <> BB.intDec builderSize
            <> BB.stringUtf8 endOfLine
            <> builder
          else mempty


-- | Format a list of tags as etags file.  Tags from the same file must be
-- grouped together.
--
formatETagsFile :: [ETag] -> Builder
formatETagsFile =
      foldMap formatTagsFile
    . groupBy (on (==) tagFilePath)

formatTagsFileMap :: ETagMap -> Builder
formatTagsFileMap = foldMap formatTagsFile

endOfLine :: String
endOfLine = "\n"
