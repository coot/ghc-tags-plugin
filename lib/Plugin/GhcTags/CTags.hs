module Plugin.GhcTags.CTags
  ( module X
  , compareTags
  ) where

import           Plugin.GhcTags.CTags.Parser    as X
import           Plugin.GhcTags.CTags.Formatter as X

import           Plugin.GhcTags.Tag (CTag)
import qualified Plugin.GhcTags.Tag as Tag

compareTags :: CTag -> CTag -> Ordering
compareTags = Tag.compareTags
