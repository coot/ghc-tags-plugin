{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
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
  , TagFilePath (..)
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
  , TagField (..)
    -- ** Ordering and combining tags
  , compareTags
  , combineTags

  -- * Create 'Tag' from a 'GhcTag'
  , ghcTagToTag
  ) where

import           Data.Function (on)
#if   __GLASGOW_HASKELL__ < 810
import           Data.ByteString (ByteString)
#endif
import qualified Data.ByteString as BS
import           Data.Text   (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.FilePath.ByteString (RawFilePath)

-- GHC imports
#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Driver.Session (DynFlags)
#else
import           DynFlags           (DynFlags (pprUserLength))
#endif
#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Data.FastString (bytesFS)
#elif __GLASGOW_HASKELL__ >= 810
import           FastString          (bytesFS)
#else
import           FastString          (FastString (fs_bs))
#endif

#if   __GLASGOW_HASKELL__ >= 900
import           GHC.Types.SrcLoc
                              ( SrcSpan (..)
                              , srcSpanFile
                              , srcSpanStartLine
                              , srcSpanStartCol
                              )
#else
import           SrcLoc       ( SrcSpan (..)
                              , srcSpanFile
                              , srcSpanStartLine
                              , srcSpanStartCol
                              )
#endif

import           GhcTags.Ghc  ( GhcTag (..)
                              , GhcTagKind (..)
                              )
#if   __GLASGOW_HASKELL__ >= 900
import qualified GHC.Utils.Outputable as Out
#else
import qualified Outputable as Out
#endif

#if   __GLASGOW_HASKELL__ < 810
bytesFS :: FastString -> ByteString
bytesFS = fs_bs
#endif

--
-- Tag
--

-- | Promoted data type used to disntinguish 'CTAG's from 'ETAG's.
--
data TAG_KIND = CTAG | ETAG


-- | Singletons for promoted types.
--
data SingTagKind (tk :: TAG_KIND) where
    SingCTag :: SingTagKind CTAG
    SingETag :: SingTagKind ETAG


-- | 'ByteString' which encodes a tag name.
--
newtype TagName = TagName { getTagName :: Text }
  deriving (Eq, Ord, Show)


-- | When we parse a `tags` file we can eithera find no kind or recognize the
-- kind of GhcTagKind or we store the found character kind.  This allows us to
-- preserve information from parsed tags files which were not created by
-- `ghc-tags-plugin'
--
-- * 'TkTerm' - @`@
-- * 'TkFunction' - @λ@
-- * 'TkTypeConstructor' - @Λ@
-- * 'TkDataConstructor' - @c@
-- * 'TkGADTConstructor' - @g@
-- * 'TkRecordField' - @r@
-- * 'TkTypeSynonym' - @≡@
-- * 'TkTypeSignature' - @⊢@
-- * 'TkPatternSynonym' - @p@
-- * 'TkTypeClass' - @C@
-- * 'TkTypeClassMember' - @m@
-- * 'TkTypeClassInstance' - @i@
-- * 'TkTypeFamily' - @f@
-- * 'TkTypeFamilyInstance' - @F@
-- * 'TkDataTypeFamily' - @d@
-- * 'TkDataTypeFamilyInstance' - @D@
-- * 'TkForeignImport' - @I@
-- * 'TkForeignExport' - @E@
--
data TagKind (tk :: TAG_KIND) where
    TkTerm                   :: TagKind CTAG
    TkFunction               :: TagKind CTAG
    TkTypeConstructor        :: TagKind CTAG
    TkDataConstructor        :: TagKind CTAG
    TkGADTConstructor        :: TagKind CTAG
    TkRecordField            :: TagKind CTAG
    TkTypeSynonym            :: TagKind CTAG
    TkTypeSignature          :: TagKind CTAG
    TkPatternSynonym         :: TagKind CTAG
    TkTypeClass              :: TagKind CTAG
    TkTypeClassMember        :: TagKind CTAG
    TkTypeClassInstance      :: TagKind CTAG
    TkTypeFamily             :: TagKind CTAG
    TkTypeFamilyInstance     :: TagKind CTAG
    TkDataTypeFamily         :: TagKind CTAG
    TkDataTypeFamilyInstance :: TagKind CTAG
    TkForeignImport          :: TagKind CTAG
    TkForeignExport          :: TagKind CTAG
    CharKind                 :: !Char -> TagKind CTAG
    NoKind                   :: TagKind tk

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
      -- | Precise addres: line and column.  This is what we infer from @GHC@
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
      TagLine :: !Int -> TagAddress tk

      -- | A tag address can be just an ex command.
      --
      TagCommand :: !ExCommand -> TagAddress CTAG

      -- | etags file format allows to discard the address
      --
      NoAddress :: TagAddress ETAG


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

-- | Unit of data associated with a tag.  Vim natively supports `file:` and
-- `kind:` tags but it can display any other tags too.
--
data TagField = TagField {
      fieldName  :: Text,
      fieldValue :: Text
    }
  deriving (Eq, Ord, Show)


-- | File field; tags which contain 'fileField' are called static (aka static
-- in @C@), such tags are only visible in the current file)
--
fileField :: TagField
fileField = TagField { fieldName = "file", fieldValue = "" }


-- | Ctags specific list of 'TagField's.
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

newtype TagFilePath = TagFilePath { getRawFilePath :: Text }
  deriving (Ord, Show)

instance Eq TagFilePath where
    (TagFilePath a) == (TagFilePath b) = a == b

-- | Tag record.  For either ctags or etags formats.  It is either filled with
-- information parsed from a tags file or from *GHC* ast.
--
data Tag (tk :: TAG_KIND) = Tag
  { tagName       :: !TagName
    -- ^ name of the tag
  , tagKind       :: !(TagKind tk)
    -- ^ ctags specifc field, which classifies tags
  , tagFilePath   :: !TagFilePath
    -- ^ source file path; it might not be normalised.
  , tagAddr       :: !(TagAddress tk)
    -- ^ address in source file
  , tagDefinition :: !(TagDefinition tk)
    -- ^ etags specific field; only tags read from emacs tags file contain this
    -- field.
  , tagFields     :: !(TagFields tk)
    -- ^ ctags specific field
  }
  deriving (Show)

instance Eq (Tag tk) where
    t0 == t1 = on (==) tagName t0 t1
            && on (==) tagKind t0 t1
            && on (==) tagFilePath t0 t1
            && on (==) tagAddr t0 t1
            && on (==) tagDefinition t0 t1
            && on (==) tagFields t0 t1


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
compareTags :: forall (tk :: TAG_KIND). Ord (TagAddress tk)
            => Tag tk -> Tag tk -> Ordering
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
      getTkClass :: Tag tk -> Maybe (TagKind tk)
      getTkClass t = case tagKind t of
        TkTypeClass              -> Just TkTypeClass
        TkTypeClassInstance      -> Just TkTypeClassInstance
        TkTypeFamily             -> Just TkTypeFamily
        TkTypeFamilyInstance     -> Just TkTypeFamilyInstance
        TkDataTypeFamily         -> Just TkDataTypeFamily
        TkDataTypeFamilyInstance -> Just TkDataTypeFamilyInstance
        _                        -> Nothing



-- | Combine tags from a single /GHC/ module with tags read from a tags file
-- with respect to the given ordering function, e.g. 'GhcTags.CTags.orderTags'
-- or 'GhcTags.ETags.orderTags'.
--
-- This is performance crtitical function.  Tags from the first list are
-- assumeed to be from the same file.
--
-- complexity: /O(max n m)/
--
combineTags :: (Tag tk -> Tag tk -> Ordering)
            -> RawFilePath
            -> [Tag tk] -> [Tag tk] -> [Tag tk]
combineTags compareFn modPath = go
  where
    go as@(a : as') bs@(b : bs')
      | modPath `BS.isSuffixOf` Text.encodeUtf8 (getRawFilePath (tagFilePath b))
      = go as bs'
      | otherwise = case a `compareFn` b of
          LT -> a : go as' bs
          EQ -> a : go as' bs'
          GT -> b : go as  bs'
    go [] bs = filter (\b -> not (modPath `BS.isSuffixOf` Text.encodeUtf8 (getRawFilePath (tagFilePath b)))) bs
    go as [] = as
    {-# INLINE go #-}


--
--  GHC interface
--

-- | Create a 'Tag' from 'GhcTag'.
--
ghcTagToTag :: SingTagKind tk -> DynFlags
            -> GhcTag -> Maybe (Tag tk)
#if   __GLASGOW_HASKELL__ >= 902
ghcTagToTag sing _dynFlags GhcTag { gtSrcSpan, gtTag, gtKind, gtIsExported, gtFFI } =
#else
ghcTagToTag sing  dynFlags GhcTag { gtSrcSpan, gtTag, gtKind, gtIsExported, gtFFI } =
#endif
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
#if   __GLASGOW_HASKELL__ >= 900
      RealSrcSpan realSrcSpan _ ->
#else
      RealSrcSpan realSrcSpan ->
#endif
        Just $ Tag
          { tagName       = TagName (Text.decodeUtf8 gtTag)
          , tagFilePath   = TagFilePath
                          $ Text.decodeUtf8
                          $ bytesFS
                          $ srcSpanFile realSrcSpan

          , tagAddr       =
              case sing of
                SingETag -> TagLine    (srcSpanStartLine realSrcSpan)
                SingCTag -> TagLineCol (srcSpanStartLine realSrcSpan)
                                       (srcSpanStartCol realSrcSpan)

          , tagKind       =
              case sing of
                SingCTag -> fromGhcTagKind gtKind
                SingETag -> NoKind

          , tagDefinition = NoTagDefinition

          , tagFields     = (    staticField
                              <> ffiField
                              <> kindField
                            ) sing
          }

  where
    fromGhcTagKind :: GhcTagKind -> CTagKind
    fromGhcTagKind = \case
      GtkTerm                      -> TkTerm
      GtkFunction                  -> TkFunction
      GtkTypeConstructor {}        -> TkTypeConstructor
      GtkDataConstructor {}        -> TkDataConstructor
      GtkGADTConstructor {}        -> TkGADTConstructor
      GtkRecordField               -> TkRecordField
      GtkTypeSynonym {}            -> TkTypeSynonym
      GtkTypeSignature {}          -> TkTypeSignature
      GtkTypeKindSignature {}      -> TkTypeSignature
      GtkPatternSynonym            -> TkPatternSynonym
      GtkTypeClass                 -> TkTypeClass
      GtkTypeClassMember {}        -> TkTypeClassMember
      GtkTypeClassInstance {}      -> TkTypeClassInstance
      GtkTypeFamily {}             -> TkTypeFamily
      GtkTypeFamilyInstance {}     -> TkTypeFamilyInstance
      GtkDataTypeFamily {}         -> TkDataTypeFamily
      GtkDataTypeFamilyInstance {} -> TkDataTypeFamilyInstance
      GtkForeignImport             -> TkForeignImport
      GtkForeignExport             -> TkForeignExport

    -- static field (wheather term is exported or not)
    staticField :: SingTagKind tk -> TagFields tk
    staticField = \case
      SingETag -> NoTagFields
      SingCTag ->
        TagFields $
          if gtIsExported
            then mempty
            else [fileField]

    -- ffi field
    ffiField :: SingTagKind tk -> TagFields tk
    ffiField = \case
      SingETag -> NoTagFields
      SingCTag ->
        TagFields $
          case gtFFI of
            Nothing  -> mempty
            Just ffi -> [TagField "ffi" (Text.pack ffi)]


    -- 'TagFields' from 'GhcTagKind'
    kindField :: SingTagKind tk -> TagFields tk
    kindField = \case
      SingETag -> NoTagFields
      SingCTag ->
        case gtKind of
          GtkTypeClassInstance hsType ->
            mkField "instance" hsType

          GtkTypeFamily (Just (hsTyVars, hsKind)) ->
            TagFields
              [ TagField
                 { fieldName  = kindFieldName
                 , fieldValue = Text.intercalate " -> " (render `map` hsTyVars)
                             <> case hsTyVars of
                                  []      ->           either render render hsKind
                                  (_ : _) -> " -> " <> either render render hsKind

                 }
              ]
          GtkTypeFamilyInstance decl ->
            TagFields
              [ TagField
                  { fieldName = typeFieldName
                  , fieldValue = render decl
                  }
              ]

          GtkDataTypeFamily (Just hsKind) ->
            mkField kindFieldName hsKind

          GtkDataTypeFamilyInstance (Just hsKind) ->
            mkField kindFieldName hsKind

          GtkTypeSignature hsSigWcType ->
            mkField typeFieldName hsSigWcType

          GtkTypeSynonym hsType ->
            mkField typeFieldName hsType

          GtkTypeConstructor (Just hsKind) ->
            mkField kindFieldName hsKind

          GtkDataConstructor decl ->
            TagFields
              [TagField
                { fieldName  = termFieldName
                , fieldValue = render decl
                }]

          GtkGADTConstructor decl ->
            TagFields
              [TagField
                { fieldName  = termFieldName
                , fieldValue = render decl
                }]

          GtkTypeClassMember hsType ->
            mkField typeFieldName hsType

          _ -> mempty


    kindFieldName, typeFieldName, termFieldName :: Text
    kindFieldName = "Kind" -- "kind" is reserverd
    typeFieldName = "type"
    termFieldName = "term"

    --
    -- fields
    --

    mkField :: Out.Outputable p => Text -> p -> TagFields CTAG
    mkField fieldName  p =
      TagFields
        [ TagField
            { fieldName
            , fieldValue = render p
            }]

    render :: Out.Outputable p => p -> Text
    render hsType =
        Text.intercalate " " -- remove all line breaks, tabs and multiple spaces
      . Text.words
      . Text.pack
#if   __GLASGOW_HASKELL__ >= 902
      . Out.renderWithContext
          Out.defaultSDocContext { Out.sdocStyle = Out.mkErrStyle Out.neverQualify }
      . Out.ppr
      $ hsType
#elif __GLASGOW_HASKELL__ >= 900
      $ Out.renderWithStyle
          (Out.initSDocContext
            dynFlags
            (Out.setStyleColoured False
              $ Out.mkErrStyle Out.neverQualify))
          (Out.ppr hsType)
          
#else
      $ Out.renderWithStyle
          (dynFlags { pprUserLength = 1 })
          (Out.ppr hsType)
          (Out.setStyleColoured False
            $ Out.mkErrStyle dynFlags Out.neverQualify)
#endif
