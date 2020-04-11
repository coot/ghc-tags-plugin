{-# LANGUAGE RankNTypes  #-}
{-# OPTIONS -Wno-orphans #-}

module Main (main) where

import           Control.Exception
import           Control.DeepSeq
import           Control.Monad.State.Strict
import           Data.ByteString (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Builder as BB
import           Data.Either (rights)
import           Data.Foldable (traverse_)
import           Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as Text
import           System.IO
import           System.FilePath.ByteString (RawFilePath)

import qualified Pipes               as Pipes
import qualified Pipes.Attoparsec    as Pipes.AP
import qualified Pipes.ByteString    as Pipes.BS

import           Criterion
import           Criterion.Main

import           GhcTags.Tag
import           GhcTags.Stream
import qualified GhcTags.CTag as CTag

evalListWith :: (forall b. a -> b -> b) -> [a] -> ()
evalListWith _seq_ [] = ()
evalListWith seq_ (a : as) = a `seq_` (evalListWith seq_ as) `seq` ()

evalEither :: Either a b -> x -> x
evalEither (Left a) x = a `seq` x
evalEither (Right b) x = b `seq` x

evalTags :: Either String [Either CTag.Header CTag] -> ()
evalTags = either (`seq` ()) (evalListWith evalEither)

newtype TagsNF = TagsNF [CTag]

instance NFData TagsNF where
    rnf (TagsNF tags) = evalListWith seq tags

main :: IO ()
main = defaultMain
  [ bgroup "Parse tags"
    [ -- 381 tags
      env (BS.readFile "test/golden/io-sim-classes.tags") $ \bs ->
      bench "parse io-sim-classes.tags" $
        whnfAppIO (fmap evalTags . CTag.parseTagsFile) bs

    , -- 6767 tags
      env (BS.readFile "test/golden/ouroboros-consensus.tags") $ \bs ->
      bench "parse ouroboros-consensus.tags" $
        whnfAppIO (fmap evalTags . CTag.parseTagsFile) bs

    , -- 12549 tags
      env (BS.readFile "bench/data.tags") $ \bs ->
      bench "data.tags" $
        whnfAppIO (fmap evalTags . CTag.parseTagsFile) bs

    , -- 23741 tags
      env (BS.readFile "test/golden/vim.tags") $ \bs ->
      bench "parse vim.tags" $
        whnfAppIO (fmap evalTags . CTag.parseTagsFile) bs
    ]
  , bgroup "read parse & format"
    [ bench "io-sim-classes.tags" $
        nfIO $ benchReadParseFormat "test/golden/io-sim-classes.tags"
    , bench "ouroboros-consensus.tags" $
        nfIO $ benchReadParseFormat "test/golden/ouroboros-consensus.tags"
    , bench "data.tags" $
        nfIO $ benchReadParseFormat "bench/data.tags"
    , bench "vim.tags" $
        nfIO $ benchReadParseFormat "test/golden/vim.tags"
    ]
  , bgroup "stream parse & format"
    [ bench "io-sim-classes.tags" $
        nfIO $ benchStreamParseFormat "test/golden/io-sim-classes.tags"
    , bench "ouroboros-consensus.tags" $
        nfIO $ benchStreamParseFormat "test/golden/ouroboros-consensus.tags"
    , bench "data.tags" $
        nfIO $ benchStreamParseFormat "bench/data.tags"
    , bench "vim.tags" $
        nfIO $ benchStreamParseFormat "test/golden/vim.tags"
    ]
    , bgroup "end-to-end"
      [ env
          (do
            bs <- BS.readFile "test/golden/io-sim-classes.tags"
            Right tags  <- fmap (mapMaybe (either (const Nothing) Just))
                           <$> CTag.parseTagsFile bs
            return (encodeTagFilePath (tagFilePath (head tags)), TagsNF tags)
          )
          $ \ ~(modPath, TagsNF tags) ->
            bgroup "small"
              [ bench "streamTags" (whnfAppIO (benchStreamTags "test/golden/vim.tags" modPath) tags)

              , bench "readTags" (whnfAppIO (benchReadTags "test/golden/vim.tags" modPath) tags)
              ]
      , env
          (do
            bs <- BS.readFile "test/golden/ouroboros-network.tags"
            Right tags  <- fmap (mapMaybe (either (const Nothing) Just))
                           <$> CTag.parseTagsFile bs
            return (encodeTagFilePath (tagFilePath (head tags)), TagsNF tags)
          )
          $ \ ~(modPath, TagsNF tags) ->
            bgroup "medium"
              [ bench "streamTags" (whnfAppIO (benchStreamTags "test/golden/vim.tags" modPath) tags)

              , bench "readTags" (whnfAppIO (benchReadTags "test/golden/vim.tags" modPath) tags)
              ]
      ]


  ]


benchReadParseFormat :: FilePath -> IO BSL.ByteString
benchReadParseFormat path = do
    bs <- BS.readFile path
    res  <- CTag.parseTagsFile bs
    case res of
      Left err   -> throwIO (userError err)
      Right tags -> pure $ BB.toLazyByteString (CTag.formatTagsFile tags)


benchStreamParseFormat :: FilePath -> IO ()
benchStreamParseFormat fp =
    withFile "/dev/null" WriteMode $ \devNull -> 
      withFile fp ReadMode $ \h ->
        Pipes.void $ Pipes.runEffect $ Pipes.for
          (Pipes.AP.parsed
            CTag.parseTag
            (Pipes.BS.fromHandle h `Pipes.for` Pipes.yield))
          (\tag -> 
            (Pipes.BS.fromLazy . BB.toLazyByteString . CTag.formatTag) tag
            Pipes.>->
            Pipes.BS.toHandle devNull)


benchStreamTags :: FilePath -> RawFilePath -> [CTag] -> IO ()
benchStreamTags filePath modPath tags =
    withFile filePath ReadMode $ \readHandle -> 
      withFile "/tmp/bench.stream.tags" WriteMode $ \writeHandle -> do
        let producer :: Pipes.Producer ByteString IO ()
            producer = void (Pipes.BS.fromHandle readHandle)

            -- gags pipe
            pipe :: Pipes.Effect (StateT [CTag] IO) ()
            pipe =
              Pipes.for
                (Pipes.hoist Pipes.lift
                  $ tagParser
                      (either (const Nothing) Just <$> CTag.parseTagLine)
                      producer)
                (runCombineTagsPipe writeHandle CTag.compareTags CTag.formatTag modPath)
        tags' <- execStateT (Pipes.runEffect pipe) tags
        traverse_ (BSL.hPut writeHandle . BB.toLazyByteString . CTag.formatTag) tags'


benchReadTags :: FilePath -> RawFilePath -> [CTag] -> IO ()
benchReadTags filePath modPath tags = do
     withFile filePath ReadMode $ \readHandle -> 
       withFile "/tmp/bench.stream.tags" WriteMode $ \writeHandle -> do
         Right tags' <-
           BS.hGetContents readHandle >>= CTag.parseTagsFile
         let tags'' = combineTags CTag.compareTags modPath tags (rights tags')
         BB.hPutBuilder writeHandle (CTag.formatTagsFile (Right `map` tags''))

encodeTagFilePath :: TagFilePath -> RawFilePath
encodeTagFilePath = Text.encodeUtf8 . getRawFilePath
