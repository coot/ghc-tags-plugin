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
    , combineTagsPipe
    , runCombineTagsPipe
    ) where

#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
import           Control.Monad.State.Strict
#else
import           Control.Monad.State.Strict hiding (void)
#endif
import           Data.ByteString (ByteString)
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Functor (void, ($>))
import qualified Data.Text.Encoding as Text
import           System.IO

import           Pipes ((>->), (~>))
import qualified Pipes as Pipes
import qualified Pipes.Lift as Pipes
import qualified Pipes.Attoparsec as Pipes.AP
import qualified Pipes.ByteString as Pipes.BS

import           GhcTags.Tag


-- | Parse a stream of tags, coming from a 'Text' producer.
--
tagParser :: MonadIO m
          => Parser (Maybe (Tag tk))
          -- ^ Parse a single tag.  For Vim this returns should parse a single
          -- line and return the tag, e.g  'parseTagLine'.
          -> Pipes.Producer ByteString m ()
          -> Pipes.Producer (Tag tk) m ()
tagParser parser producer = void $
  Pipes.for
    (Pipes.AP.parsed parser producer)
    $ \case
      -- ignore header lines
      Just tag -> Pipes.yield tag
      Nothing  -> pure ()


-- | Streaming version of 'GhcTags.Tag.combineTags'.
--
combineTagsPipe
    :: forall m (tk :: TAG_KIND).  Applicative m
    => (Tag tk -> Tag tk -> Ordering)
    -> RawFilePath -- ^ file path from which the new tags were obtained, it should be normalised
    -> Tag tk      -- ^ tag read from disc
    -> [Tag tk]    -- ^ new tags
    -> Pipes.Producer (Tag tk) m [Tag tk]
combineTagsPipe compareFn modPath = go
  where
    modPath' = rawFilePathToBS modPath
    go :: Tag tk -> [Tag tk]
       -> Pipes.Producer (Tag tk) m [Tag tk]

    -- omitt all the tags which point to 'modPath'
    --
    -- note: we check that 'tagFilePath' ends with 'modPath', which is
    -- a relative path from the corresponding cabal file.
    go tag as
      | modPath' `BS.isSuffixOf` Text.encodeUtf8 (getRawFilePath (tagFilePath tag))
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
