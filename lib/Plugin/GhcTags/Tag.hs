{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Plugin.GhcTags.Tag
  ( -- * Tag
    Tag (..)
  , TagName (..)
  , TagFile (..)
  , TagKind (..)
  , charToTagKind
  , TagField (..)
  , ghcTagToTag
  , TagsMap
  , mkTagsMap
  ) where

import           Data.List (sort)
import           Data.Map  (Map)
import qualified Data.Map as Map
import           Data.Text   (Text)
import qualified Data.Text.Encoding as Text

-- GHC imports
import           FastString   ( FastString (..)
                              )
import           SrcLoc       ( SrcSpan (..)
                              , srcSpanFile
                              , srcSpanStartLine
                              )

import           Plugin.GhcTags.Generate
                              ( GhcTag (..)
                              , TagKind (..)
                              , TagField (..)
                              , charToTagKind
                              )

--
-- Tag
--


-- | 'ByteString' which encodes a tag name.
--
newtype TagName = TagName { getTagName :: Text }
  deriving newtype (Eq, Ord, Show)


-- | 'ByteString' which encodes a tag file.
--
newtype TagFile = TagFile { getTagFile :: Text }
  deriving newtype (Eq, Ord, Show)


-- | Simple Tag record.  For the moment on tag name, tag file and line numbers
-- are supported.
--
-- TODO: expand to support column numbers and extra information.
--
data Tag = Tag
  { tagName   :: !TagName
  , tagKind   :: !(Maybe TagKind)
  , tagFile   :: !TagFile
  , tagAddr   :: !(Either Int Text)
  , tagFields :: ![TagField]
  }
  deriving (Ord, Eq, Show)


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { gtSrcSpan, gtTag, gtKind, gtFields } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName   = TagName (Text.decodeUtf8 $ fs_bs gtTag)
                   , tagFile   = TagFile (Text.decodeUtf8 $ fs_bs (srcSpanFile realSrcSpan))
                   , tagAddr   = Left (srcSpanStartLine realSrcSpan)
                   , tagKind   = Just gtKind
                   , tagFields = gtFields
                   }


--
-- TagsMap
--


type TagsMap = Map TagFile [Tag]

-- | Map from TagName to list of tags.  This will be useful when updating tags.
-- We will just need to merge dictionaries.
--
mkTagsMap :: [Tag] -> TagsMap
mkTagsMap =
      fmap sort
    . Map.fromListWith (<>)
    . map (\t -> (tagFile t, [t]))
