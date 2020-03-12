{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Plugin.GhcTags.Tag
  ( -- * Tag
    Tag (..)
  , compareTags
  , tagFilePath
  , TagName (..)
  , TagFile (..)
  , TagKind (..)
  , GhcKind (..)
  , charToGhcKind
  , ghcKindToChar
  , TagField (..)
  , ghcTagToTag
  , combineTags
  ) where

import           Data.Function (on)
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
  deriving (Eq, Ord, Show)


-- | 'ByteString' which encodes a tag file.
--
newtype TagFile = TagFile { getTagFile :: String }
  deriving (Eq, Ord, Show)

tagFilePath :: Tag -> FilePath
tagFilePath = getTagFile . tagFile


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


-- | Total order relation on 'Tag' elements.
--
-- It sorts type classes / type families ('TkTypeClass', 'TkTypeFamily',
-- 'TkDataTypeFamily')  before instances ('TkTypeClassInstance',
-- 'TkTypeFamilyInstance', 'TkDataTypeFamilyInstance'); but also (as a side
-- effect of keeping transitivity property) it will put type classes and their
-- instances before other kinds.
--
compareTags :: Tag -> Tag -> Ordering
compareTags t0 t1 | on (/=) tagName t0 t1 = on compare tagName t0 t1

                  -- sort type classes / type families before their instances,
                  -- and take precendence over a file where they are defined.
                  -- 
                  -- This will also sort type classes and instances before any
                  -- other terms.
                  | on (/=) getTkClass t0 t1 = on compare getTkClass t0 t1

                  | on (/=) tagFile t0 t1 = on compare tagFile t0 t1
                  | on (/=) tagAddr t0 t1 = on compare tagAddr t0 t1
                  | on (/=) tagKind t0 t1 = on compare tagKind t0 t1

                  -- this is not compatible with 'Eq' intsance, but we are not
                  -- defining a 'Ord' instance!
                  | otherwise             = EQ
    where
      getTkClass :: Tag -> Maybe GhcKind
      getTkClass t = case tagKind t of
        GhcKind TkTypeClass              -> Just TkTypeClass
        GhcKind TkTypeClassInstance      -> Just TkTypeClassInstance
        GhcKind TkTypeFamily             -> Just TkTypeFamily
        GhcKind TkTypeFamilyInstance     -> Just TkTypeFamilyInstance
        GhcKind TkDataTypeFamily         -> Just TkDataTypeFamily
        GhcKind TkDataTypeFamilyInstance -> Just TkDataTypeFamilyInstance
        _                                -> Nothing


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


-- This is crtitical function for perfomance.  Tags from the first list are
-- assumeed to be from the same file.
--
-- complexity: /O(max n m)/
combineTags :: [Tag] -> [Tag] -> [Tag]
combineTags []          ts1 = ts1
combineTags ts0@(t : _) ts1 = go ts0 ts1
  where
    modPath = tagFilePath t

    go as@(a : as') bs@(b : bs')
      | tagFilePath b == modPath = go as bs'
      | otherwise = case a `compareTags` b of
          LT -> a : go as' bs
          EQ -> a : go as' bs'
          GT -> b : go as  bs'
    go [] bs = filter (\b -> tagFilePath b /= modPath) bs
    go as [] = as
    {-# INLINE go #-}
