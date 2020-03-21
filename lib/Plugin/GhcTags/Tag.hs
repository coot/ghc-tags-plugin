{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Plugin.GhcTags.Tag
  ( -- * Tag
    TAG_KIND (..)
  , SingTagKind (..)
  , Tag (..)
  , ETag
  , CTag
  , compareTags
  , TagName (..)
  , ExCommand (..)
  , TagAddress (..)
  , CTagAddress
  , ETagAddress
  , TagKind (..)
  , CTagKind
  , ETagKind
  , TagDefinition (..)
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
                              , srcSpanStartCol
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

data TAG_KIND = CTAG | ETAG

-- | Singletons
--
data SingTagKind (tk :: TAG_KIND) where
    SingCTag :: SingTagKind CTAG
    SingETag :: SingTagKind ETAG


-- | 'ByteString' which encodes a tag name.
--
newtype TagName = TagName { getTagName :: Text }
  deriving (Eq, Ord, Show)


-- | When we parse a `tags` file we can eithera find no kind or recognize the
-- kind of GhcKind or we store the found character kind.  This allows us to
-- preserve information from parsed tags files which were not created by
-- `ghc-tags-plugin'
--
data TagKind (tk :: TAG_KIND) where
    GhcKind  :: !GhcKind -> TagKind CTAG
    CharKind :: !Char -> TagKind CTAG
    NoKind   :: TagKind tk

type CTagKind = TagKind CTAG

type ETagKind = TagKind ETAG

instance Eq (TagKind tk) where
    GhcKind k == GhcKind k' = k  == k'
    CharKind c == CharKind c' = c == c'
    NoKind == NoKind = True
    _ == _ = False

instance Ord (TagKind tk) where
    GhcKind k `compare` GhcKind k' = k `compare` k'
    GhcKind {} `compare` _ = LT
    _ `compare` GhcKind {} = GT

    CharKind c  `compare` CharKind c' = c `compare` c'
    CharKind {} `compare` _ = LT
    _ `compare` CharKind {} = GT

    NoKind `compare` NoKind = EQ

instance Show (TagKind tk) where
    show (GhcKind k)  = "GhcKind " ++ show k
    show (CharKind c) = "CharKind " ++ show c
    show NoKind       = "NoKind" 


newtype ExCommand = ExCommand { getExCommand :: Text }
  deriving (Eq, Ord, Show)


-- | Tag address, either from a parsed file or from Haskell's AST>
--
data TagAddress (tk :: TAG_KIND) where
      -- | Precise addres: line and column.  This is what we infer from Haskell
      -- AST.
      --
      -- TODO: rename to 'TagLineOffset' (the second argument is a byte offset)
      --
      TagLineCol :: !Int -> !Int -> TagAddress tk

      -- | vim can only encode a column as an ex-command; This is what we can
      -- parse from a tag file.
      --
      TagLine :: !Int -> TagAddress CTAG

      -- | A tag address can be just an ex command.
      --
      TagCommand :: !ExCommand -> TagAddress CTAG


type CTagAddress = TagAddress CTAG

type ETagAddress = TagAddress ETAG

instance Eq (TagAddress tk) where
    TagLineCol l0 c0 == TagLineCol l1 c1 = l0 == l1 && c0 == c1
    TagLine l0       == TagLine    l1    = l0 == l1
    TagCommand c0    == TagCommand c1    = c0 == c1
    _                == _                = False

instance Ord (TagAddress CTAG) where
    TagLineCol l0 c0 `compare` TagLineCol l1 c1 =
      l0 `compare` l1 <> c0 `compare` c1
    TagLineCol l0 _  `compare` TagLine l1 =
      l0 `compare` l1
    TagLine l0 `compare` TagLineCol l1 _ =
      l0 `compare` l1
    TagLineCol {} `compare` TagCommand {} = LT
    TagCommand {} `compare` TagLineCol {} = GT

    TagLine l0 `compare` TagLine l1 =
      l0 `compare` l1
    TagLine {} `compare` TagCommand {} = LT
    TagCommand {} `compare` TagLine {} = GT

    TagCommand {} `compare` TagCommand {} = EQ

instance Ord (TagAddress ETAG) where
    TagLineCol l0 c0 `compare` TagLineCol l1 c1 =
      l0 `compare` l1 <> c0 `compare` c1

instance Show (TagAddress tk) where
    show (TagLineCol l c) = "TagLineCol " ++ show l ++ " " ++ show c
    show (TagLine    l)   = "TagLine " ++ show l
    show (TagCommand c)   = "TagCommand " ++ show c

-- | This is etags specific field.
--
data TagDefinition =
      TagDefinition !Text
    | NoTagDefinition
  deriving (Eq, Show)


-- | Simple Tag record.  For the moment on tag name, tag file and line numbers
-- are supported.
--
-- TODO: expand to support column numbers and extra information.
--
data Tag (tk :: TAG_KIND) = Tag
  { tagName       :: !TagName
  , tagKind       :: !(TagKind tk)
  , tagFilePath   :: !FilePath
  , tagAddr       :: !(TagAddress tk)
  , tagDefinition :: !TagDefinition
  , tagFields     :: ![TagField]
  }
  deriving (Eq, Show)

type CTag = Tag CTAG

type ETag = Tag ETAG


-- | Total order relation on 'Tag' elements.
--
-- It sorts type classes / type families ('TkTypeClass', 'TkTypeFamily',
-- 'TkDataTypeFamily')  before instances ('TkTypeClassInstance',
-- 'TkTypeFamilyInstance', 'TkDataTypeFamilyInstance'); but also (as a side
-- effect of keeping transitivity property) it will put type classes and their
-- instances before other kinds.
--
compareTags :: forall (tk :: TAG_KIND). Ord (TagAddress tk) => Tag tk -> Tag tk -> Ordering
compareTags t0 t1 = on compare tagName t0 t1
                    -- sort type classes / type families before their instances,
                    -- and take precendence over a file where they are defined.
                    -- 
                    -- This will also sort type classes and instances before any
                    -- other terms.
                 <> on compare getTkClass  t0 t1
                 <> on compare tagFilePath t0 t1
                 <> on compare tagAddr     t0 t1
                 <> on compare tagKind     t0 t1

    where
      getTkClass :: Tag tk -> Maybe GhcKind
      getTkClass t = case tagKind t of
        GhcKind TkTypeClass              -> Just TkTypeClass
        GhcKind TkTypeClassInstance      -> Just TkTypeClassInstance
        GhcKind TkTypeFamily             -> Just TkTypeFamily
        GhcKind TkTypeFamilyInstance     -> Just TkTypeFamilyInstance
        GhcKind TkDataTypeFamily         -> Just TkDataTypeFamily
        GhcKind TkDataTypeFamilyInstance -> Just TkDataTypeFamilyInstance
        _                                -> Nothing


ghcTagToTag :: SingTagKind tk -> GhcTag -> Maybe (Tag tk)
ghcTagToTag sing GhcTag { gtSrcSpan, gtTag, gtKind, gtFields } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName       = TagName (Text.decodeUtf8 $ fs_bs gtTag)
                   , tagFilePath   = Text.unpack $ Text.decodeUtf8 $ fs_bs (srcSpanFile realSrcSpan)
                   , tagAddr       = TagLineCol (srcSpanStartLine realSrcSpan)
                                                (srcSpanStartCol realSrcSpan)
                   , tagKind       = case sing of
                                       SingCTag -> GhcKind gtKind
                                       SingETag -> NoKind
                   , tagDefinition = NoTagDefinition
                   , tagFields     = gtFields
                   }


-- This is crtitical function for perfomance.  Tags from the first list are
-- assumeed to be from the same file.
--
-- complexity: /O(max n m)/
combineTags :: (Tag tk -> Tag tk -> Ordering)
            -> FilePath
            -> [Tag tk] -> [Tag tk] -> [Tag tk]
combineTags compare_ modPath ts0 ts1 = go ts0 ts1
  where
    go as@(a : as') bs@(b : bs')
      | tagFilePath b == modPath = go as bs'
      | otherwise = case a `compare_` b of
          LT -> a : go as' bs
          EQ -> a : go as' bs'
          GT -> b : go as  bs'
    go [] bs = filter (\b -> tagFilePath b /= modPath) bs
    go as [] = as
    {-# INLINE go #-}
