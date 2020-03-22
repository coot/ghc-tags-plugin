{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module GhcTags.Tag
  ( -- * Tag
    TAG_KIND (..)
  , SingTagKind (..)
  , Tag (..)
  , ETag
  , CTag
    -- ** Tag fields
  , TagName (..)
  , ExCommand (..)
  , TagAddress (..)
  , CTagAddress
  , ETagAddress
  , TagKind (..)
  , CTagKind
  , ETagKind
  , TagDefinition (..)
  , TagFields (..)
  , CTagFields
  , ETagFields
  , GhcKind (..)
  , TagField (..)
    -- ** Ordering and combining tags
  , compareTags
  , combineTags

  -- * GHC inteface
  , ghcTagToTag
  , charToGhcKind
  , ghcKindToChar
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

import           GhcTags.Ghc  ( GhcTag (..)
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

deriving instance Eq   (TagKind tk)
deriving instance Ord  (TagKind tk)
deriving instance Show (TagKind tk)


newtype ExCommand = ExCommand { getExCommand :: Text }
  deriving (Eq, Ord, Show)


-- | Tag address, either from a parsed file or from Haskell's AST>
--
data TagAddress (tk :: TAG_KIND) where
      -- | Precise addres: line and column.  This is what we infer from Haskell
      -- AST.
      --
      -- The two arguments are line number and either column number or offset
      -- from the begining of the file.
      --
      TagLineCol :: !Int -> !Int -> TagAddress tk

      -- | ctags can only use range ex-commands as an address (or a sequence of
      -- them separated by `;`). We parse line number specifically, since they
      -- are useful for ordering tags.
      --
      TagLine :: !Int -> TagAddress CTAG

      -- | A tag address can be just an ex command.
      --
      TagCommand :: !ExCommand -> TagAddress CTAG


-- | 'CTag' addresses.
--
type CTagAddress = TagAddress CTAG

-- | 'ETag' addresses.
--
type ETagAddress = TagAddress ETAG


deriving instance Eq   (TagAddress tk)
deriving instance Ord  (TagAddress tk)
deriving instance Show (TagAddress tk)


-- | Emacs tags specific field.
--
data TagDefinition (tk :: TAG_KIND) where
      TagDefinition   :: !Text -> TagDefinition ETAG
      NoTagDefinition :: TagDefinition tk

deriving instance Show (TagDefinition tk)
deriving instance Eq   (TagDefinition tk)


-- | Ctags specific list of fields.
--
data TagFields (tk :: TAG_KIND) where
    NoTagFields :: TagFields ETAG

    TagFields   :: ![TagField]
                -> TagFields CTAG

deriving instance Show (TagFields tk)
deriving instance Eq   (TagFields tk)
instance Semigroup (TagFields tk) where
    NoTagFields   <> NoTagFields   = NoTagFields
    (TagFields a) <> (TagFields b) = TagFields (a ++ b)
instance Monoid (TagFields CTAG) where
    mempty = TagFields mempty
instance Monoid (TagFields ETAG) where
    mempty = NoTagFields

type CTagFields = TagFields CTAG
type ETagFields = TagFields ETAG


-- | Tag record.  For either ctags or etags formats.  It is either filled with
-- information parsed from a tags file or from *GHC* ast.
--
data Tag (tk :: TAG_KIND) = Tag
  { tagName       :: !TagName
    -- ^ name of the tag
  , tagKind       :: !(TagKind tk)
    -- ^ ctags specifc field, which classifies tags
  , tagFilePath   :: !FilePath
    -- ^ source file path
  , tagAddr       :: !(TagAddress tk)
    -- ^ address in source file
  , tagDefinition :: !(TagDefinition tk)
    -- ^ etags specific field
  , tagFields     :: !(TagFields tk)
    -- ^ ctags specific field
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
-- It satisfies the following properties:
--
-- * anti-symmetry
-- * reflexivity
-- * transitivity
-- * partial consistency with 'Eq' instance: 
--
--   prop> a == b => compareTags a b == EQ
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



-- | Combine tags from a tags file with tags from *GHC* ast.
--
-- This is crtitical function for perfomance.  Tags from the first list are
-- assumeed to be from the same file.
--
-- complexity: /O(max n m)/
--
combineTags :: (Tag tk -> Tag tk -> Ordering)
            -> FilePath
            -> [Tag tk] -> [Tag tk] -> [Tag tk]
combineTags compareFn modPath = go
  where
    go as@(a : as') bs@(b : bs')
      | tagFilePath b == modPath = go as bs'
      | otherwise = case a `compareFn` b of
          LT -> a : go as' bs
          EQ -> a : go as' bs'
          GT -> b : go as  bs'
    go [] bs = filter (\b -> tagFilePath b /= modPath) bs
    go as [] = as
    {-# INLINE go #-}


--
--  GHC interface
--

--  | Create a 'Tag' from 'GhcTag'
--
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
                   , tagFields     = case sing of
                                       SingCTag -> TagFields gtFields
                                       SingETag -> NoTagFields
                   }
