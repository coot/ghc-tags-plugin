module Plugin.GhcTags.ETags
  ( module X
  , compareTags
  ) where

import           Data.Function (on)

import           Plugin.GhcTags.ETags.Formatter as X
import           Plugin.GhcTags.ETags.Parser    as X

import           Plugin.GhcTags.Tag ( Tag (..)
                                    , ETag
                                    , TagAddress (..)
                                    , tagFilePath
                                    )


-- | Order 'ETag's according to filename & byteOffset
--
compareTags :: ETag -> ETag -> Ordering
compareTags t0 t1 =
       on compare tagFilePath t0 t1
    <> on compare (\Tag {tagAddr = TagLineCol _ byteOffset} -> byteOffset) t0 t1
    <> on compare tagName t0 t1
