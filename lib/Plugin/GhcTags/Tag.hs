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
  , compareTags
  , TagName (..)
  , TagFile (..)
  , TagKind (..)
  , GhcKind (..)
  , charToGhcKind
  , ghcKindToChar
  , TagField (..)
  , ghcTagToTag
  , TagsMap
  , mkTagsMap
  ) where

import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Map  (Map)
import qualified Data.Map as Map
import           Data.Text   (Text)
import qualified Data.Text as Text
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
                              , GhcKind (..)
                              , TagField (..)
                              , charToGhcKind
                              , ghcKindToChar
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
newtype TagFile = TagFile { getTagFile :: String }
  deriving newtype (Eq, Ord, Show)


-- | When we parse a `tags` file we can eithera find no kind or recognize the
-- kind of GhcKind or we store the found character kind.  This allows us to
-- preserve information from parsed tags files which were not created by
-- `ghc-tags-plugin'
--
data TagKind
  = GhcKind  !GhcKind
  | CharKind !Char
  | NoKind
  deriving (Eq, Ord, Show)

-- | Simple Tag record.  For the moment on tag name, tag file and line numbers
-- are supported.
--
-- TODO: expand to support column numbers and extra information.
--
data Tag = Tag
  { tagName   :: !TagName
  , tagKind   :: !TagKind
  , tagFile   :: !TagFile
  , tagAddr   :: !(Either Int Text)
  , tagFields :: ![TagField]
  }
  deriving (Eq, Show)

compareTags :: Tag -> Tag -> Ordering
compareTags t0 t1 | on (/=) tagName t0 t1 = on compare tagName t0 t1

                  -- sort type classes / type families before their instances,
                  -- and take precendence over a file where they are defined.
                  | tagKind t0 == GhcKind TkTypeClass
                    &&
                    tagKind t1 == GhcKind TkTypeClassInstance
                    = LT

                  | tagKind t1 == GhcKind TkTypeClass
                    &&
                    tagKind t0 == GhcKind TkTypeClassInstance
                    = GT

                  | tagKind t0 == GhcKind TkTypeFamily
                    &&
                    tagKind t1 == GhcKind TkTypeFamilyInstance
                    = LT

                  | tagKind t1 == GhcKind TkTypeFamily
                    &&
                    tagKind t0 == GhcKind TkTypeFamilyInstance
                    = GT

                  | tagKind t0 == GhcKind TkDataTypeFamily
                    &&
                    tagKind t1 == GhcKind TkDataTypeFamilyInstance
                    = LT

                  | tagKind t1 == GhcKind TkDataTypeFamily
                    &&
                    tagKind t0 == GhcKind TkDataTypeFamilyInstance
                    = GT

                  | on (/=) tagFile t0 t1 = on compare tagFile t0 t1
                  | on (/=) tagAddr t0 t1 = on compare tagAddr t0 t1
                  | on (/=) tagKind t0 t1 = on compare tagKind t0 t1

                  -- this is not compatible with 'Eq' intsance, but we are not
                  -- defining a 'Ord' instance!
                  | otherwise             = EQ


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { gtSrcSpan, gtTag, gtKind, gtFields } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName   = TagName (Text.decodeUtf8 $ fs_bs gtTag)
                   , tagFile   = TagFile (Text.unpack $ Text.decodeUtf8 $ fs_bs (srcSpanFile realSrcSpan))
                   , tagAddr   = Left (srcSpanStartLine realSrcSpan)
                   , tagKind   = GhcKind gtKind
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
      fmap (sortBy compareTags)
    . Map.fromListWith (<>)
    . map (\t -> (tagFile t, [t]))
