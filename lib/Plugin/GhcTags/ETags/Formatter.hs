{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Simple etags formatter. See <https://en.wikipedia.org/wiki/Ctags#Etags>
--
module Plugin.GhcTags.ETags.Formatter where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import           Data.List (groupBy)
import           Data.Function (on)
import           Data.Foldable (foldl')
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import           Plugin.GhcTags.Tag
import           Plugin.GhcTags.Utils (endOfLine)


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

getByteOffset
    :: [ByteString]
    -> Int -- ^ line
    -> Int -- ^ column
    -> Int -- ^ byteoffset
getByteOffset linesBS line col =
    foldl' (\len l -> len + fromIntegral (BS.length l) + 1) 0 (take line linesBS) + col

formatTag :: ETag -> BuilderWithSize
formatTag Tag {tagName, tagAddr, tagDefinition} =
           flip BuilderWithSize tagSize $
        -- TODO: get access to the original line or pretty print original
        -- declaration
           BB.byteString tagDefinitionBS
        <> BB.charUtf8 '\DEL' -- or '\x7f'
        <> BB.byteString tagNameBS
        <> BB.charUtf8 '\SOH' -- or '\x01'
        <> BB.intDec lineNo
        <> BB.charUtf8 ','
        <> BB.intDec (succ lineNo)
        <> BB.stringUtf8 endOfLine
  where
    lineNo = case tagAddr of
      TagLineCol l _ -> l

    tagNameBS = Text.encodeUtf8 . getTagName $ tagName
    tagNameSize = BS.length tagNameBS

    tagDefinitionBS = case tagDefinition of
      NoTagDefinition   -> tagNameBS
      TagDefinition def -> Text.encodeUtf8 def
    tagDefinitionSize = BS.length tagDefinitionBS

    tagSize =
        3 -- delimiters: '\DEL', '\SOH', ','
      + tagNameSize
      + tagDefinitionSize
      + (length $ show lineNo)
      + (length $ show (succ lineNo))
      + (length $ endOfLine)


-- | The precondition is that all the tags come frome the same file.
--
formatTagsFile :: [ETag] -> Builder
formatTagsFile [] = mempty
formatTagsFile ts@(Tag {tagFile} : _) =
    case foldMap formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        if builderSize > 0
          then
               BB.charUtf8 '\x0c'
            <> BB.stringUtf8 endOfLine
            <> (BB.byteString . Text.encodeUtf8 . Text.pack . getTagFile $ tagFile)
            <> BB.charUtf8 ','
            <> BB.intDec builderSize
            <> BB.stringUtf8 endOfLine
            <> builder
          else mempty


-- | Format a list of tags as etags file.  Tags from the same file must be
-- groupped together.
--
formatETagsFile :: [ETag] -> Builder
formatETagsFile =
      foldMap formatTagsFile 
    . groupBy (on (==) tagFile)
