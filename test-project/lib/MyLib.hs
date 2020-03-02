{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class MyLength a where
    myLength :: a -> Int

instance MyLength String where
    myLength = length

class GetHeader blk where
  data family Header blk :: *
  getHeader :: blk -> Header blk

instance GetHeader Int where
    data Header Int = HeaderInt Int
    getHeader = HeaderInt

data family X (a :: *) :: *

data instance X Int
  = XInt Int
  | XString String


data Record a = Record {
    recordId    :: Int,
    recordField :: a
  }
