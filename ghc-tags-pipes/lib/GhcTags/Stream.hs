{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Parse and combine a stream of tags.
--
module GhcTags.Stream
    ( tagParser
    , tagParserWithWarnings
    , ParsingError (..)
    , combineTagsStream
    , combineTagsPipe
    , runCombineTagsPipe
    ) where

#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
import           Control.Monad.State.Strict
#else
import           Control.Monad.State.Strict hiding (void)
#endif
import           Control.Exception (throwIO)
import           Data.ByteString (ByteString)
import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import qualified Data.Text.Encoding as Text
import           System.IO

import           Pipes ((>->), (~>))
import qualified Pipes as Pipes
import qualified Pipes.Lift as Pipes
import qualified Pipes.Attoparsec as Pipes.AP
import           Pipes.Attoparsec (ParsingError (..))
import qualified Pipes.ByteString as Pipes.BS

import           GhcTags.Tag
import qualified GhcTags.Utils as Utils


-- | Parse a stream of tags, coming from a 'Text' producer.
-- Skips malformed lines and continues parsing. Use 'tagParserWithWarnings' to
-- report skipped records. I/O exceptions still propagate to the consumer.
--
tagParser :: MonadIO m
          => Parser (Maybe (Tag tk))
          -- ^ Parse a single tag.  For Vim this returns should parse a single
          -- line and return the tag, e.g  'parseTagLine'.
          -> Pipes.Producer ByteString m ()
          -> Pipes.Producer (Tag tk) m ()
tagParser = tagParserWithWarnings (const (pure ()))


-- | Recover a stream of tags and report every skipped line.
tagParserWithWarnings :: MonadIO m
                      => (String -> m ())
                      -> Parser (Maybe (Tag tk))
                      -> Pipes.Producer ByteString m ()
                      -> Pipes.Producer (Tag tk) m ()
tagParserWithWarnings report parser producer = do
      Pipes.for
        (Pipes.AP.parsed (Utils.parseLine parser) producer)
        (\case
          Left err -> Pipes.lift (report err)
          -- ignore header lines
          Right (Just tag) -> Pipes.yield tag
          Right Nothing   -> pure ())
  >>= \case
        Left (err, _) -> liftIO (throwIO err)
        Right ()      -> pure ()


-- | Merge a sorted stream of old tags with sorted new tags from one module.
-- Replaces that module's old tags and removes duplicate output tags, including
-- when the old stream is empty. The comparison must be consistent with equality.
combineTagsStream
    :: forall m (tk :: TAG_KIND). Monad m
    => (Tag tk -> Tag tk -> Ordering)
    -> RawFilePath -- ^ normalised module path, relative to the tags file directory
    -> [Tag tk]   -- ^ new tags, sorted by the supplied comparison
    -> Pipes.Producer (Tag tk) m () -- ^ sorted old tags
    -> Pipes.Producer (Tag tk) m ()
combineTagsStream compareFn modPath new old =
    merged >-> deduplicateTags compareFn modPath
  where
    merged :: Pipes.Producer (Tag tk) m ()
    merged = do
      ((), remaining) <- Pipes.runStateP new $
        Pipes.for (Pipes.hoist Pipes.lift old)
          (\tag -> Pipes.stateP $ fmap ((),) . combineTagsPipe compareFn modPath tag)
      traverse_ Pipes.yield remaining


-- Equal tags have equal sorting keys, so only the current comparison group
-- needs to be remembered. Keep distinct tags with equal keys: compareTags
-- does not compare fields or definitions.  Only deduplicate groups that
-- contain at least one new (modPath) tag; old-only groups are preserved as-is
-- because etags loses tagKind on round-trip, making formerly-distinct tags
-- look identical after parsing.
deduplicateTags :: Monad m
                => (Tag tk -> Tag tk -> Ordering)
                -> RawFilePath
                -> Pipes.Pipe (Tag tk) (Tag tk) m r
deduplicateTags compareFn modPath = go [] False
  where
    modPath' = rawFilePathToBS modPath
    isNew t  = modPath' == Text.encodeUtf8 (getRawFilePath (tagFilePath t))
    go seen hasNew = do
      tag <- Pipes.await
      let inGroup = case seen of
            previous : _ | compareFn previous tag == EQ -> True
            _ -> False
      if inGroup
        then let hasNew' = hasNew || isNew tag
             in if hasNew' && tag `elem` seen
                then go seen hasNew'
                else Pipes.yield tag >> go (tag : seen) hasNew'
        else Pipes.yield tag >> go [tag] (isNew tag)


-- | One merge step for an old tag. This does not deduplicate the complete
-- output; use 'combineTagsStream' for a complete merge.
--
combineTagsPipe
    :: forall m (tk :: TAG_KIND).  Applicative m
    => (Tag tk -> Tag tk -> Ordering)
    -> RawFilePath -- ^ normalised module path, relative to the tags file directory
    -> Tag tk      -- ^ tag read from disc
    -> [Tag tk]    -- ^ new tags
    -> Pipes.Producer (Tag tk) m [Tag tk]
combineTagsPipe compareFn modPath = go
  where
    modPath' = rawFilePathToBS modPath
    go :: Tag tk -> [Tag tk]
       -> Pipes.Producer (Tag tk) m [Tag tk]

    -- Omit only tags pointing to the exact module path. Both paths are
    -- normalised and relative to the tags file directory.
    go tag as
      | modPath' == Text.encodeUtf8 (getRawFilePath (tagFilePath tag))
      = pure as

    go tag as@(a : as')
      | otherwise = case a `compareFn` tag of
          LT -> Pipes.yield a >> go tag as'
          EQ -> Pipes.yield a $> as'
          GT -> Pipes.yield tag $> as

    go tag [] = Pipes.yield tag $> []


-- | run 'combineTagsPipe' taking care of the state.
--
runCombineTagsPipe
    :: MonadIO m
    => Handle
    -> (Tag tk -> Tag tk -> Ordering)
    -> (Tag tk -> Builder)
    -> RawFilePath
    -> Tag tk
    -> Pipes.Effect (StateT [Tag tk] m) ()
runCombineTagsPipe writeHandle compareFn formatTag modPath =
       (\tag -> Pipes.stateP $ fmap ((),) . combineTagsPipe compareFn modPath tag)
    ~> Pipes.yield . BS.toLazyByteString . formatTag
    ~> (\bs -> Pipes.BS.fromLazy bs)
    ~> (\bs -> Pipes.yield bs >-> Pipes.BS.toHandle writeHandle)
