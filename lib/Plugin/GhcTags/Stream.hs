{-# LANGUAGE LambdaCase          #-}
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
          => Parser (Maybe Tag)
          -- ^ Parse a single tag.  For Vim this returns should parse a single
          -- line and return the tag, e.g  'parseTagLine'.
          -> Pipes.Producer Text m ()
          -> Pipes.Producer Tag m ()
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
    :: forall m.
       Applicative m
    => Tag   -- ^ tag read from disc
    -> [Tag] -- ^ new tags
    -> Pipes.Producer Tag m [Tag]
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
    :: MonadIO m
    => Handle
    -> (Tag -> Builder)
    -> Tag
    -> Pipes.Effect (StateT [Tag] m) ()
runCombineTagsPipe writeHandle formatTag =
      (\tag -> Pipes.stateP $ fmap ((),) . combineTagsPipe tag)
    ~> Pipes.yield . BS.toLazyByteString . formatTag 
    ~> Pipes.BS.fromLazy
    ~> \bs -> Pipes.yield bs >-> Pipes.BS.toHandle writeHandle
