module GhcTags.CTag
  ( module X
  , compareTags
  ) where

import           Data.Function (on)

import           GhcTags.CTag.Header    as X
import           GhcTags.CTag.Parser    as X
import           GhcTags.CTag.Formatter as X
import           GhcTags.CTag.Utils     as X

import           GhcTags.Tag (CTag)
import qualified GhcTags.Tag as Tag

-- | Compare ctags by their output keys. Columns are omitted by the ctags
-- format, so fresh 'Tag.TagLineCol' addresses must compare like parsed 'Tag.TagLine'
-- addresses. This also preserves ordering after formatting and parsing.
compareTags :: CTag -> CTag -> Ordering
compareTags = Tag.compareTags `on` stripColumn
