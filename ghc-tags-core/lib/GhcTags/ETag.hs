{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module GhcTags.ETag
  ( module X
  , compareTags
  ) where

import           Data.Function (on)

import           GhcTags.ETag.Formatter as X
import           GhcTags.ETag.Parser    as X

import           GhcTags.Tag ( Tag (..)
                             , ETag
                             , TagAddress (..)
                             , tagFilePath
                             )


-- | Order 'ETag's according to filename & byteOffset
--
compareTags :: ETag -> ETag -> Ordering
compareTags t0 t1 =
       on compare tagFilePath t0 t1
    <> on compare (\Tag {tagAddr} ->
                    case tagAddr of
                      TagLineCol line _ -> line
                      TagLine line      -> line
                      NoAddress         -> 0
                  ) t0 t1
    <> on compare tagName t0 t1
