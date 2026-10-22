{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser (tests) where

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Either (isRight)
import           Data.IORef
import qualified Data.Text as Text

import qualified Pipes
import qualified Pipes.Prelude as Pipes
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified GhcTags.CTag as CTag
import qualified GhcTags.ETag as ETag
import           GhcTags.Stream (tagParserWithWarnings)
import           GhcTags.Tag

import           Test.Tag.Generators (genTextNonEmpty)


tests :: TestTree
tests = testGroup "Whole-file parsing"
  [ testProperty "ctags round trip" ctagsRoundTripProp
  , testProperty "etags round trip" etagsRoundTripProp
  , testProperty "streaming ctags round trip across chunks" streamRoundTripProp
  , testProperty "ctags recovery supports all line endings across chunks" ctagsLineEndingsProp
  , testProperty "etags recovery supports all line endings" etagsLineEndingsProp
  , testProperty "ctags recover after malformed lines" ctagsMalformedProp
  , testProperty "ctags recover after malformed headers" ctagsMalformedHeaderProp
  , testProperty "etags recover within damaged sections" etagsMalformedProp
  , testProperty "etags recover between sections" etagsBetweenSectionsProp
  , testProperty "etags recover after damaged section headers" etagsSectionHeaderProp
  , testProperty "streaming ctags recover across chunks" streamMalformedProp
  , testProperty "ctags recover before a truncated final tag" ctagsTruncatedProp
  , testProperty "etags recover before a truncated final tag" etagsTruncatedProp
  , testProperty "streaming ctags recover before a truncated final tag" streamTruncatedProp
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
  recovered <- CTag.parseTagsFileRecovering bytes
  pure $ conjoin
    [ parsed === Right (map Right tags)
    , property (isRight parsedMap)
    , recovered === Right ([], map Right tags)
    ]


etagsRoundTripProp :: ParserTags -> Property
etagsRoundTripProp input = ioProperty $ do
  let tags = modelETags input
      bytes = encodeETags tags
  parsed <- ETag.parseTagsFile bytes
  parsedMap <- ETag.parseTagsFileMap bytes
  recovered <- ETag.parseTagsFileRecovering bytes
  pure $ conjoin
    [ parsed === Right tags
    , property (isRight parsedMap)
    , recovered === Right ([], tags)
    ]


streamRoundTripProp :: ParserTags -> Positive Int -> Property
streamRoundTripProp input chunkSize = ioProperty $ do
  let tags = modelCTags input
      headers = BL.toStrict $ BB.toLazyByteString $
        CTag.formatHeader (CTag.Header CTag.FileFormat Nothing 2 "extended format")
  result <- parseStream chunkSize (headers <> encodeCTags tags)
  pure $ result === ([], tags)


ctagsLineEndingsProp :: ParserTags -> NonNegative Int -> Positive Int -> Property
ctagsLineEndingsProp input (NonNegative ending) chunkSize = ioProperty $ do
  let eol = ["\n", "\r", "\r\n"] !! (ending `mod` 3)
      convert = BS.intercalate eol . BS.split 10 . BS.filter (/= 13)
      ctags = modelCTags input
      cbytes = convert (encodeCTags ctags)
  cparsed <- CTag.parseTagsFileRecovering cbytes
  streamed <- parseStream chunkSize cbytes
  pure $ conjoin
    [ cparsed === Right ([], map Right ctags)
    , streamed === ([], ctags)
    ]


etagsLineEndingsProp :: ParserTags -> NonNegative Int -> Property
etagsLineEndingsProp input (NonNegative ending) = ioProperty $ do
  let eol = ["\n", "\r", "\r\n"] !! (ending `mod` 3)
      convert = BS.intercalate eol . BS.split 10 . BS.filter (/= 13)
      etags = modelETags input
  parsed <- ETag.parseTagsFileRecovering (convert (encodeETags etags))
  pure $ parsed === Right ([], etags)


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
  -- Remove record delimiters; other bytes exercise malformed UTF-8 as well.
  -- At most one tab leaves a required ctags separator missing, even on shrinking.
  BS.filter (`notElem` [1, 9, 10, 12, 13, 127]) bytes
    <> if includePath then "\tA.hs\n" else "\n"


ctagsMalformedProp :: ParserTags -> NonNegative Int -> MalformedLine -> Property
ctagsMalformedProp input position invalid = ioProperty $ do
  let bytes = damagedFile encodeCTags (modelCTags input) position invalid
  parsed <- CTag.parseTagsFileRecovering bytes
  pure $ counterexample (show bytes) $
    fmap (\(warnings, tags) -> (length warnings, tags)) parsed
      === Right (1, map Right (modelCTags input))


ctagsMalformedHeaderProp :: ParserTags -> NonNegative Int -> Bool -> Positive Int -> Property
ctagsMalformedHeaderProp input position emptyComment chunkSize = ioProperty $ do
  let tags = modelCTags input
      badHeader = "!_TAG_FILE_FORMAT\t2\t/" <> if emptyComment then "\n" else "unterminated\n"
      -- Insert the raw header to exercise its delimiters and comment syntax.
      (before, after) = splitAt (getNonNegative position `mod` (length tags + 1)) tags
      headerBytes = encodeCTags before <> badHeader <> encodeCTags after
  parsed <- CTag.parseTagsFileRecovering headerBytes
  (warnings, streamed) <- parseStream chunkSize headerBytes
  pure $ counterexample (show headerBytes) $ conjoin
    [ fmap (\(errors, recovered) -> (length errors, recovered)) parsed === Right (1, map Right tags)
    , (length warnings, streamed) === (1, tags)
    ]


etagsMalformedProp :: ParserTags -> NonNegative Int -> MalformedLine -> Property
etagsMalformedProp input position invalid = ioProperty $ do
  let tags = modelETags input
      -- Keep one section header, and insert damage among that section's rows.
      (marker, rest) = BS.break (== 10) (encodeETags tags)
      (header, _) = BS.break (== 10) (BS.drop 1 rest)
      prefix = if null tags then mempty else marker <> "\n" <> header <> "\n"
      bytes = prefix <> damagedFile encodeETagRows tags position invalid
  parsed <- ETag.parseTagsFileRecovering bytes
  pure $ counterexample (show bytes) $
    fmap (\(warnings, recovered) -> (length warnings, recovered)) parsed === Right (1, tags)


encodeETagRows :: [ETag] -> ByteString
encodeETagRows = BL.toStrict . BB.toLazyByteString . ETag.builder . foldMap ETag.formatTag


etagsBetweenSectionsProp :: ParserTags -> NonNegative Int -> MalformedLine -> Property
etagsBetweenSectionsProp input position invalid = ioProperty $ do
  let tags = modelETags input
      bytes = damagedFile encodeETags tags position invalid
  parsed <- ETag.parseTagsFileRecovering bytes
  pure $ counterexample (show bytes) $
    fmap (\(warnings, recovered) -> (length warnings, recovered)) parsed === Right (1, tags)


etagsSectionHeaderProp :: ParserTags -> ParserTags -> ParserTags -> Bool -> Bool -> Property
etagsSectionHeaderProp before damaged after missingHeader badMarker = ioProperty $ do
  let beforeTags = modelETags before
      afterTags = [tag { tagFilePath = TagFilePath "B.hs" } | tag <- modelETags after]
      marker = if badMarker then "\x0c damaged\n" else "\x0c\n"
      badHeader = marker <> if missingHeader then mempty else "bad section header\n"
      bytes = encodeETags beforeTags <> badHeader <> encodeETagRows (modelETags damaged)
                <> encodeETags afterTags
  parsed <- ETag.parseTagsFileRecovering bytes
  pure $ counterexample (show bytes) $
    fmap (\(warnings, recovered) -> (not (null warnings), recovered)) parsed
      === Right (True, beforeTags ++ afterTags)


streamMalformedProp :: ParserTags -> NonNegative Int -> Positive Int -> MalformedLine -> Property
streamMalformedProp input position chunkSize invalid = ioProperty $ do
  let bytes = damagedFile encodeCTags (modelCTags input) position invalid
  (warnings, tags) <- parseStream chunkSize bytes
  pure $ counterexample (show bytes) $ (length warnings, tags) === (1, modelCTags input)


ctagsTruncatedProp :: ParserTags -> Property
ctagsTruncatedProp input = ioProperty $ do
  let tags = modelCTags (nonEmptyTags input)
      bytes = BS.init $ encodeCTags tags
  parsed <- CTag.parseTagsFileRecovering bytes
  pure $ fmap (\(warnings, recovered) -> (length warnings, recovered)) parsed
    === Right (1, map Right (init tags))


etagsTruncatedProp :: ParserTags -> Property
etagsTruncatedProp input = ioProperty $ do
  let tags = modelETags (nonEmptyTags input)
      bytes = BS.init $ encodeETags tags
  parsed <- ETag.parseTagsFileRecovering bytes
  pure $ fmap (\(warnings, recovered) -> (length warnings, recovered)) parsed
    === Right (1, init tags)


streamTruncatedProp :: ParserTags -> Positive Int -> Property
streamTruncatedProp input chunkSize = ioProperty $ do
  let tags = modelCTags (nonEmptyTags input)
      bytes = BS.init $ encodeCTags tags
  (warnings, recovered) <- parseStream chunkSize bytes
  pure $ (length warnings, recovered) === (1, init tags)


nonEmptyTags :: ParserTags -> ParserTags
nonEmptyTags (ParserTags tags) = ParserTags (("last", Positive 1) : tags)


parseStream :: Positive Int -> ByteString -> IO ([String], [CTag])
parseStream (Positive size) bytes = do
  warnings <- newIORef []
  tags <- Pipes.toListM $
    tagParserWithWarnings (\err -> modifyIORef' warnings (err :))
      (either (const Nothing) Just <$> CTag.parseTagLine) $
      Pipes.each (chunks bytes)
  errors <- reverse <$> readIORef warnings
  pure (errors, tags)
  where
    chunks bs
      | BS.null bs = [BS.empty]
      | otherwise =
          let (prefix, rest) = BS.splitAt (1 + size `mod` 64) bs
          in BS.empty : prefix : chunks rest
