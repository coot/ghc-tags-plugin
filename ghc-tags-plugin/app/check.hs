-- | Utility program which checks the size of tags file.
--
-- It's a like `wc` but using `lock` file, so we don't get intermediate
-- results.
--
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           System.FilePath
import           System.IO
import           System.Environment

import           Plugin.GhcTags.FileLock


main :: IO ()
main = do
    file :_ <- getArgs
    withFileLock (lockFile file) ExclusiveLock $ \_h -> do
      numOfLines <- length . BSC.lines <$> BS.readFile file
      putStrLn (show numOfLines)
  where
    lockFile file = case splitFileName file of
      (dir, name) -> dir </> "." ++ name ++ ".lock"
