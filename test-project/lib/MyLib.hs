{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MyLib 
  ( someFunc
  , MyLength (..)
  , GetHeader (Header)
  , Header(HeaderInt)
  , X (XInt)
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class MyLength a where
    myLength :: a -> Int

instance MyLength String where
    myLength = length

class GetHeader blk where
  data family Header blk :: *
  type family Argument blk :: *
  getHeader :: Argument blk -> Header blk

instance GetHeader Int where
    data Header Int = HeaderInt Int
    type Argument Int = Int
    getHeader = HeaderInt

data family X (a :: *) :: *

type family Y a :: *

data instance X Int
  = XInt Int
  | XString String


data Record a = Record {
    recordId    :: Int,
    recordField :: a
  }
