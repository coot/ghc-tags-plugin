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
    <> on compare (\Tag {tagAddr = TagLine line} -> line) t0 t1
    <> on compare tagName t0 t1
