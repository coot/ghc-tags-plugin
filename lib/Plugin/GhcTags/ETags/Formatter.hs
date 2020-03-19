{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Simple etags formatter. See <https://en.wikipedia.org/wiki/Ctags#Etags>
--
module Plugin.GhcTags.ETags.Formatter where

import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
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


formatTag :: Tag -> BuilderWithSize
formatTag Tag {tagName, tagAddr} =
    case mLineNo of
      Nothing     -> mempty
      Just lineNo -> flip BuilderWithSize (tagSize lineNo) $
        -- TODO: get access to the original line or pretty print original
        -- declaration
           tagNameBuilder
        <> BB.charUtf8 '\DEL' -- or '\x7f'
        <> tagNameBuilder
        <> BB.charUtf8 '\SOH' -- or '\x01'
        <> BB.intDec lineNo
        <> BB.charUtf8 ','
        <> BB.intDec (succ lineNo)
        <> BB.stringUtf8 endOfLine
  where
    mLineNo = case tagAddr of
      TagLineCol l _ -> Just l
      TagLine    l   -> Just l
      TagCommand _   -> Nothing

    tagNameBS      = Text.encodeUtf8 . getTagName $ tagName
    tagNameBuilder = BB.byteString tagNameBS
    tagNameSize    = BS.length     tagNameBS

    tagSize lineNo =
        2 * tagNameSize
      + 3
      + (length $ show lineNo)
      + (length $ show (succ lineNo))
      + (length $ endOfLine)


-- | The precondition is that all the tags come frome the same file.
--
formatTagsFile :: [Tag] -> Builder
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
