{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Simple etags formatter. See <https://en.wikipedia.org/wiki/Ctags#Etags>
--
module GhcTags.ETag.Formatter
  ( withByteOffset
  , formatETagsFile 
  , formatTagsFile
  , formatTag
  , BuilderWithSize (..)
  ) where

import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import           Data.List (groupBy)
import           Data.Function (on)
import           Data.Foldable (foldl')
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

computeByteOffset
    :: [Int]
    -- ^ lengths of lines
    -> ETagAddress
    -> ETagAddress
computeByteOffset ll (TagLineCol line col) = TagLineCol line byteOffset
  where
    byteOffset =
        foldl' (+) 0 (take (pred line) ll)
      + col

withByteOffset :: [Int] -> ETag -> ETag
withByteOffset ll tag@Tag { tagAddr } = tag { tagAddr = computeByteOffset ll tagAddr }

formatTag :: ETag -> BuilderWithSize
formatTag Tag {tagName, tagAddr = TagLineCol lineNr byteOffset, tagDefinition} =
           flip BuilderWithSize tagSize $
        -- TODO: get access to the original line or pretty print original
        -- declaration
           case tagDefinition of
              NoTagDefinition   -> BB.byteString tagNameBS
              TagDefinition def -> BB.byteString (Text.encodeUtf8 def)
        <> BB.charUtf8 '\DEL' -- or '\x7f'
        <> case tagDefinition of
             NoTagDefinition -> mempty
             TagDefinition _ ->
                   BB.byteString tagNameBS
                <> BB.charUtf8 '\SOH' -- or '\x01'
        <> BB.intDec lineNr
        <> BB.charUtf8 ','
        <> BB.intDec byteOffset
        <> BB.stringUtf8 endOfLine
  where
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
      + (length $ show lineNr)
      + (length $ show byteOffset)
      + (length $ endOfLine)


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
-- groupped together.
--
formatETagsFile :: [ETag] -> Builder
formatETagsFile =
      foldMap formatTagsFile 
    . groupBy (on (==) tagFilePath)


endOfLine :: String
endOfLine = "\n"
