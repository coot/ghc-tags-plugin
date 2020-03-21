{-# OPTIONS -Wno-orphans #-}

module Main (main) where

import           Control.Exception
import           Control.DeepSeq
import           Control.Monad.State.Strict
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Builder as BB
import           Data.Foldable (traverse_)
import           Data.Text            (Text)
import qualified Data.Text.Encoding as Text
import           System.IO

import qualified Pipes               as Pipes
import qualified Pipes.Attoparsec    as Pipes.AP
import qualified Pipes.ByteString    as Pipes.BS
import qualified Pipes.Text.Encoding as Pipes.Text

import           Criterion
import           Criterion.Main

import           Plugin.GhcTags.Tag
import           Plugin.GhcTags.Stream
import qualified Plugin.GhcTags.CTags as CTags

evalList :: [a] -> ()
evalList [] = ()
evalList (a : as) = a `seq` evalList as `seq` ()

evalTags :: Either String [CTag] -> ()
evalTags = either (`seq` ()) evalList

newtype TagsNF = TagsNF [CTag]

instance NFData TagsNF where
    rnf (TagsNF tags) = evalList tags

instance NFData TagFile where
    rnf (TagFile p) = rnf p

main :: IO ()
main = defaultMain
  [ bgroup "Parse tags"
    [ -- 381 tags
      env (Text.decodeUtf8 <$> BS.readFile "test/golden/io-sim-classes.tags") $ \text ->
      bench "parse io-sim-classes.tags" $
        whnfAppIO (fmap evalTags . CTags.parseTagsFile) text

    , -- 6767 tags
      env (Text.decodeUtf8 <$> BS.readFile "test/golden/ouroboros-consensus.tags") $ \text ->
      bench "parse ouroboros-consensus.tags" $
        whnfAppIO (fmap evalTags . CTags.parseTagsFile) text

    , -- 12549 tags
      env (Text.decodeUtf8 <$> BS.readFile "bench/data.tags") $ \text ->
      bench "data.tags" $
        whnfAppIO (fmap evalTags . CTags.parseTagsFile) text

    , -- 23741 tags
      env (Text.decodeUtf8 <$> BS.readFile "test/golden/vim.tags") $ \text ->
      bench "parse vim.tags" $
        whnfAppIO (fmap evalTags . CTags.parseTagsFile) text
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
            text <- Text.decodeUtf8 <$> BS.readFile "test/golden/io-sim-classes.tags"
            Right tags  <- CTags.parseTagsFile text
            return (tagFile (head tags), TagsNF tags)
          )
          $ \ ~(modPath, TagsNF tags) ->
            bgroup "small"
              [ bench "streamTags" (whnfAppIO (benchStreamTags "test/golden/vim.tags" modPath) tags)

              , bench "readTags" (whnfAppIO (benchReadTags "test/golden/vim.tags" modPath) tags)
              ]
      , env
          (do
            text <- Text.decodeUtf8 <$> BS.readFile "test/golden/ouroboros-network.tags"
            Right tags  <- CTags.parseTagsFile text
            return (tagFile (head tags), TagsNF tags)
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
    text <- Text.decodeUtf8 <$> BS.readFile path
    res  <- CTags.parseTagsFile text
    case res of
      Left err   -> throwIO (userError err)
      Right tags -> pure $ BB.toLazyByteString (CTags.formatTagsFile tags)


benchStreamParseFormat :: FilePath -> IO ()
benchStreamParseFormat fp =
    withFile "/dev/null" WriteMode $ \devNull -> 
      withFile fp ReadMode $ \h ->
        Pipes.void $ Pipes.runEffect $ Pipes.for
          (Pipes.AP.parsed
            CTags.parseTag
            (Pipes.BS.fromHandle h `Pipes.for` (Pipes.yield . Text.decodeUtf8)))
          (\tag -> 
            (Pipes.BS.fromLazy . BB.toLazyByteString . CTags.formatTag) tag
            Pipes.>->
            Pipes.BS.toHandle devNull)


benchStreamTags :: FilePath -> TagFile -> [CTag] -> IO ()
benchStreamTags filePath modPath tags =
    withFile filePath ReadMode $ \readHandle -> 
      withFile "/tmp/bench.stream.tags" WriteMode $ \writeHandle -> do
        let producer :: Pipes.Producer Text IO ()
            producer =
              void $ Pipes.Text.decodeUtf8
                           (Pipes.BS.fromHandle readHandle)

            -- gags pipe
            pipe :: Pipes.Effect (StateT [CTag] IO) ()
            pipe =
              Pipes.for
                (Pipes.hoist Pipes.lift $ tagParser CTags.parseTagLine producer)
                (runCombineTagsPipe writeHandle CTags.compareTags CTags.formatTag modPath)
        tags' <- execStateT (Pipes.runEffect pipe) tags
        traverse_ (BSL.hPut writeHandle . BB.toLazyByteString . CTags.formatTag) tags'


benchReadTags :: FilePath -> TagFile -> [CTag] -> IO ()
benchReadTags filePath modPath tags = do
     withFile filePath ReadMode $ \readHandle -> 
       withFile "/tmp/bench.stream.tags" WriteMode $ \writeHandle -> do
         Right tags' <-
           Text.decodeUtf8 <$> BS.hGetContents readHandle
           >>= CTags.parseTagsFile
         let tags'' = combineTags CTags.compareTags modPath tags tags'
         BB.hPutBuilder writeHandle (CTags.formatTagsFile tags'')
