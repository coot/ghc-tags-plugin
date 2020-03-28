module GhcTags.CTag
  ( module X
  , compareTags
  ) where

import           GhcTags.CTag.Header    as X
import           GhcTags.CTag.Parser    as X
import           GhcTags.CTag.Formatter as X
import           GhcTags.CTag.Utils     as X

import           GhcTags.Tag (CTag)
import qualified GhcTags.Tag as Tag

-- | A specialisation of 'GhcTags.Tag.compareTags' to 'CTag's.
--
compareTags :: CTag -> CTag -> Ordering
compareTags = Tag.compareTags
