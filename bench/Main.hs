module Main (main) where

import           Prelude
import           Control.Exception
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Builder as BB
import           Data.Text            (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           System.IO

import qualified Pipes            as Pipes
import qualified Pipes.Attoparsec as Pipes.AP
import qualified Pipes.ByteString as Pipes.BS

import           Criterion
import           Criterion.Main

import           Plugin.GhcTags.Tag
import           Plugin.GhcTags.Vim.Parser
import           Plugin.GhcTags.Vim.Formatter

evalList :: [a] -> ()
evalList [] = ()
evalList (a : as) = a `seq` evalList as `seq` ()

evalTags :: Either String [Tag] -> ()
evalTags = either (`seq` ()) evalList

main :: IO ()
main = defaultMain
  [ bgroup "Parse tags"
    [ -- 381 tags
      env (Text.decodeUtf8 <$> BS.readFile "test/golden/io-sim-classes.tags") $ \text ->
      bench "parse io-sim-classes.tags" $
        whnfAppIO (fmap evalTags . parseTagsFile) text

    , -- 6767 tags
      env (Text.decodeUtf8 <$> BS.readFile "test/golden/ouroboros-consensus.tags") $ \text ->
      bench "parse ouroboros-consensus.tags" $
        whnfAppIO (fmap evalTags . parseTagsFile) text

    , -- 12549 tags
      env (Text.decodeUtf8 <$> BS.readFile "bench/data.tags") $ \text ->
      bench "data.tags" $
        whnfAppIO (fmap evalTags . parseTagsFile) text

    , -- 23741 tags
      env (Text.decodeUtf8 <$> BS.readFile "test/golden/vim.tags") $ \text ->
      bench "parse vim.tags" $
        whnfAppIO (fmap evalTags . parseTagsFile) text
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
  ]


benchReadParseFormat :: FilePath -> IO BSL.ByteString
benchReadParseFormat path = do
    text <- Text.decodeUtf8 <$> BS.readFile path
    res  <- parseTagsFile text
    case res of
      Left err   -> throwIO (userError err)
      Right tags -> pure $ BB.toLazyByteString (formatTagsFile tags)


benchStreamParseFormat :: FilePath -> IO ()
benchStreamParseFormat fp =
    withFile "/dev/null" WriteMode $ \devNull -> 
      withFile fp ReadMode $ \h ->
        Pipes.void $ Pipes.runEffect $ Pipes.for
          (Pipes.AP.parsed
            parseTag
            (Pipes.BS.fromHandle h `Pipes.for` (Pipes.yield . Text.decodeUtf8)))
          (\tag -> 
            (Pipes.BS.fromLazy . BB.toLazyByteString . formatTag) tag
            Pipes.>->
            Pipes.BS.toHandle devNull)

