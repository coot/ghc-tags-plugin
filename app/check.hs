module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           System.IO
import           System.Environment
import           System.FileLock  ( SharedExclusive (..)
                                  , withFileLock)


main :: IO ()
main = do
    file :_ <- getArgs
    withFileLock file Exclusive $ \_ -> do
      numOfLines <- length . BSC.lines <$> BS.readFile file
      putStrLn (show numOfLines)
