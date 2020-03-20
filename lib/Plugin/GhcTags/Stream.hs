{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Plugin.GhcTags.Stream
    ( tagParser
    , combineTagsPipe
    , runCombineTagsPipe
    ) where

import           Control.Monad.State.Strict
import           Data.Attoparsec.Text  (Parser)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import           Data.Functor (($>))
import           Data.Text (Text)
import           System.IO

import           Pipes ((>->), (~>))
import qualified Pipes as Pipes
import qualified Pipes.Lift as Pipes
import qualified Pipes.Attoparsec as Pipes.AP
import qualified Pipes.ByteString as Pipes.BS

import           Plugin.GhcTags.Tag


tagParser :: MonadIO m
          => Parser (Maybe (Tag tk))
          -- ^ Parse a single tag.  For Vim this returns should parse a single
          -- line and return the tag, e.g  'parseTagLine'.
          -> Pipes.Producer Text m ()
          -> Pipes.Producer (Tag tk) m ()
tagParser parser producer = void $
  Pipes.for
    (Pipes.AP.parsed parser producer)
    $ \case
      -- ignore header lines
      Just tag -> Pipes.yield tag
      Nothing  -> pure ()


--  | 'Pipe' version of 'combineTags'.
--
combineTagsPipe
    :: forall m (tk :: TAG_KIND).
       ( Applicative m
       , Ord (TagAddress tk)
       )
    => Tag tk   -- ^ tag read from disc
    -> [Tag tk] -- ^ new tags
    -> Pipes.Producer (Tag tk) m [Tag tk]
-- TODO: if a module has no tags, we will not remove any of the pre-exisiting
-- ones.
combineTagsPipe tag0 []         = Pipes.yield tag0 $> []
combineTagsPipe tag0 ts@(t : _) = go tag0 ts
  where
    modPath = tagFilePath t

    go tag as@(a : as')
      | tagFilePath tag == modPath = pure as
      | otherwise = case a `compareTags` tag of
          LT -> Pipes.yield a >> go tag as'
          EQ -> Pipes.yield a >> go tag as'
          GT -> Pipes.yield tag $> as
    go tag []
      | tagFilePath tag == modPath = pure []
      | otherwise                  = Pipes.yield tag $> []


-- | run 'combineTagsPipe' taking care of the state.
--
runCombineTagsPipe
    :: ( MonadIO m
       , Ord (TagAddress tk)
       )
    => Handle
    -> (Tag tk -> Builder)
    -> Tag tk
    -> Pipes.Effect (StateT [Tag tk] m) ()
runCombineTagsPipe writeHandle formatTag =
      (\tag -> Pipes.stateP $ fmap ((),) . combineTagsPipe tag)
    ~> Pipes.yield . BS.toLazyByteString . formatTag 
    ~> Pipes.BS.fromLazy
    ~> \bs -> Pipes.yield bs >-> Pipes.BS.toHandle writeHandle
