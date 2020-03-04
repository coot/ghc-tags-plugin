{-# LANGUAGE GADTs #-}

module AnotherModule where

import MyLib

-- | Either a block (@blk@) or a header (@'Header' blk@). Both have the same
-- @HeaderHash blk@.
data BlockOrHeader blk b where
  Block  :: BlockOrHeader blk blk
  Header :: BlockOrHeader blk (Header blk)

function :: Int -> Int
function = undefined

type MyInt = Int
