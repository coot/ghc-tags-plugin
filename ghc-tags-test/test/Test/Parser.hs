{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser (tests) where

import           Control.Exception (try)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Either (isLeft, isRight)
import qualified Data.Text as Text

import qualified Pipes
import qualified Pipes.Prelude as Pipes
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag
import           GhcTags.Stream (ParsingError, tagParser)
import           GhcTags.Tag

import           Test.Tag.Generators (genTextNonEmpty)


tests :: TestTree
tests = testGroup "Whole-file parsing"
  [ testProperty "ctags round trip" ctagsRoundTripProp
  , testProperty "etags round trip" etagsRoundTripProp
  , testProperty "streaming ctags round trip across chunks" streamRoundTripProp
  , testProperty "ctags reject malformed lines anywhere" ctagsMalformedProp
  , testProperty "etags reject malformed lines anywhere" etagsMalformedProp
  , testProperty "streaming ctags reject malformed lines across chunks" streamMalformedProp
  , testProperty "ctags reject a truncated final tag" ctagsTruncatedProp
  , testProperty "etags reject a truncated final tag" etagsTruncatedProp
  , testProperty "streaming ctags reject a truncated final tag" streamTruncatedProp
  ]


-- A common model for both encodings. Restrict names to avoid ctags headers;
-- keep empty files and duplicate tags among the generated cases.
newtype ParserTags = ParserTags [(String, Positive Int)]
  deriving Show

instance Arbitrary ParserTags where
  arbitrary = ParserTags <$> listOf
    ((,) <$> (Text.unpack <$> genTextNonEmpty) <*> arbitrary)
  shrink (ParserTags tags) = ParserTags <$> shrinkList shrink tags


modelCTags :: ParserTags -> [CTag]
modelCTags (ParserTags tags) =
  [ Tag (TagName (Text.pack ("tag" ++ name))) NoKind (TagFilePath "A.hs")
        (TagLine line) NoTagDefinition (TagFields [])
  | (name, Positive line) <- tags
  ]


modelETags :: ParserTags -> [ETag]
modelETags (ParserTags tags) =
  [ Tag (TagName (Text.pack name)) NoKind (TagFilePath "A.hs")
        (TagLine line) NoTagDefinition NoTagFields
  | (name, Positive line) <- tags
  ]


encodeCTags :: [CTag] -> ByteString
encodeCTags = BL.toStrict . BB.toLazyByteString . foldMap CTag.formatTag


encodeETags :: [ETag] -> ByteString
encodeETags = BL.toStrict . BB.toLazyByteString . ETag.formatTagsFile


ctagsRoundTripProp :: ParserTags -> Property
ctagsRoundTripProp input = ioProperty $ do
  let tags = modelCTags input
      bytes = encodeCTags tags
  parsed <- CTag.parseTagsFile bytes
  parsedMap <- CTag.parseTagsFileMap bytes
  pure $ conjoin [parsed === Right (map Right tags), property (isRight parsedMap)]


etagsRoundTripProp :: ParserTags -> Property
etagsRoundTripProp input = ioProperty $ do
  let tags = modelETags input
      bytes = encodeETags tags
  parsed <- ETag.parseTagsFile bytes
  parsedMap <- ETag.parseTagsFileMap bytes
  pure $ conjoin [parsed === Right tags, property (isRight parsedMap)]


streamRoundTripProp :: ParserTags -> Positive Int -> Property
streamRoundTripProp input chunkSize = ioProperty $ do
  let tags = modelCTags input
      headers = BL.toStrict $ BB.toLazyByteString $
        CTag.formatHeader (CTag.Header CTag.FileFormat Nothing 2 "extended format")
  result <- parseStream chunkSize (headers <> encodeCTags tags)
  pure $ result === Right tags


-- Insert a malformed line before, between or after valid tags. Encoding the
-- two halves separately also exercises boundaries between etags sections.
damagedFile :: ([tag] -> ByteString) -> [tag] -> NonNegative Int -> MalformedLine -> ByteString
damagedFile encode tags (NonNegative position) invalid =
  let (before, after) = splitAt (position `mod` (length tags + 1)) tags
  in encode before <> encodeMalformedLine invalid <> encode after


-- Exercise missing separators after both the tag name and the file path.
data MalformedLine = MalformedLine ByteString Bool
  deriving Show

instance Arbitrary MalformedLine where
  arbitrary = MalformedLine <$> arbitrary <*> arbitrary
  shrink (MalformedLine bytes includePath) =
    [MalformedLine bytes' includePath' | (bytes', includePath') <- shrink (bytes, includePath)]


encodeMalformedLine :: MalformedLine -> ByteString
encodeMalformedLine (MalformedLine bytes includePath) =
  -- Printable ASCII excludes line breaks, tabs and etags delimiters. Appending
  -- at most one tab leaves a required ctags separator missing, even on shrinking.
  BS.filter (\byte -> byte >= 32 && byte < 127) bytes
    <> if includePath then "\tA.hs\n" else "\n"


ctagsMalformedProp :: ParserTags -> NonNegative Int -> MalformedLine -> Property
ctagsMalformedProp input position invalid = ioProperty $ do
  let bytes = damagedFile encodeCTags (modelCTags input) position invalid
  parsed <- CTag.parseTagsFile bytes
  parsedMap <- CTag.parseTagsFileMap bytes
  pure $ counterexample (show bytes) $ conjoin
    [property (isLeft parsed), property (isLeft parsedMap)]


etagsMalformedProp :: ParserTags -> NonNegative Int -> MalformedLine -> Property
etagsMalformedProp input position invalid = ioProperty $ do
  let bytes = damagedFile encodeETags (modelETags input) position invalid
  parsed <- ETag.parseTagsFile bytes
  parsedMap <- ETag.parseTagsFileMap bytes
  pure $ counterexample (show bytes) $ conjoin
    [property (isLeft parsed), property (isLeft parsedMap)]


streamMalformedProp :: ParserTags -> NonNegative Int -> Positive Int -> MalformedLine -> Property
streamMalformedProp input position chunkSize invalid = ioProperty $ do
  let bytes = damagedFile encodeCTags (modelCTags input) position invalid
  result <- parseStream chunkSize bytes
  pure $ counterexample (show result) $ property (isLeft result)


ctagsTruncatedProp :: ParserTags -> Property
ctagsTruncatedProp input = ioProperty $ do
  let bytes = BS.init $ encodeCTags $ modelCTags (nonEmptyTags input)
  parsed <- CTag.parseTagsFile bytes
  parsedMap <- CTag.parseTagsFileMap bytes
  pure $ conjoin [property (isLeft parsed), property (isLeft parsedMap)]


etagsTruncatedProp :: ParserTags -> Property
etagsTruncatedProp input = ioProperty $ do
  let bytes = BS.init $ encodeETags $ modelETags (nonEmptyTags input)
  parsed <- ETag.parseTagsFile bytes
  parsedMap <- ETag.parseTagsFileMap bytes
  pure $ conjoin [property (isLeft parsed), property (isLeft parsedMap)]


streamTruncatedProp :: ParserTags -> Positive Int -> Property
streamTruncatedProp input chunkSize = ioProperty $ do
  let bytes = BS.init $ encodeCTags $ modelCTags (nonEmptyTags input)
  result <- parseStream chunkSize bytes
  pure $ counterexample (show result) $ property (isLeft result)


nonEmptyTags :: ParserTags -> ParserTags
nonEmptyTags (ParserTags tags) = ParserTags (("last", Positive 1) : tags)


parseStream :: Positive Int -> ByteString -> IO (Either ParsingError [CTag])
parseStream (Positive size) bytes = try $ Pipes.toListM $
  tagParser (either (const Nothing) Just <$> CTag.parseTagLine) $
    Pipes.each (chunks bytes)
  where
    chunks bs
      | BS.null bs = [BS.empty]
      | otherwise =
          let (prefix, rest) = BS.splitAt (1 + size `mod` 64) bs
          in BS.empty : prefix : chunks rest
