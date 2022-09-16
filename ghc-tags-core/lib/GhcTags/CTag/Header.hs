{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module GhcTags.CTag.Header
  ( Header (..)
  , HeaderType (..)
  , SomeHeaderType (..)
  -- * Utils
  , SingHeaderType (..)
  , headerTypeSing
  ) where

import           Control.DeepSeq (NFData (..))
import           Data.Text (Text)


-- | A type safe representation of a /ctag/ header.
--
data Header where
    Header :: forall ty. (NFData ty, Show ty) =>
              { headerType     :: HeaderType ty
              , headerLanguage :: Maybe Text
              , headerArg      :: ty
              , headerComment  :: Text
              }
            -> Header

instance Eq Header where
    Header  { headerType = headerType0
            , headerLanguage = headerLanguage0
            , headerArg  = headerArg0
            , headerComment = headerComment0
            }
      ==
      Header { headerType = headerType1
             , headerLanguage = headerLanguage1
             , headerArg  = headerArg1
             , headerComment = headerComment1
             } =
        case (headerType0, headerType1) of
          (FileEncoding, FileEncoding) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (FileFormat, FileFormat) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (FileSorted, FileSorted) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (OutputMode, OutputMode) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (KindDescription, KindDescription) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (KindSeparator, KindSeparator) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramAuthor, ProgramAuthor) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramName, ProgramName) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramUrl, ProgramUrl) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramVersion, ProgramVersion) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ExtraDescription, ExtraDescription) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (FieldDescription, FieldDescription) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (PseudoTag name0, PseudoTag name1) ->
            name0 == name1 &&
            headerLanguage0 == headerLanguage1 &&
            headerArg0 == headerArg1 &&
            headerComment0 == headerComment1
          _ -> False

deriving instance Show Header

instance NFData Header where
  rnf Header {..} = rnf headerType
              `seq` rnf headerLanguage
              `seq` rnf headerArg
              `seq` rnf headerComment

-- | Enumeration of header type and values of their corresponding argument
--
data HeaderType ty where
    FileEncoding      :: HeaderType Text
    FileFormat        :: HeaderType Int
    FileSorted        :: HeaderType Int
    OutputMode        :: HeaderType Text
    KindDescription   :: HeaderType Text
    KindSeparator     :: HeaderType Text
    ProgramAuthor     :: HeaderType Text
    ProgramName       :: HeaderType Text
    ProgramUrl        :: HeaderType Text
    ProgramVersion    :: HeaderType Text

    ExtraDescription  :: HeaderType Text
    FieldDescription  :: HeaderType Text
    PseudoTag         :: Text -> HeaderType Text

deriving instance Eq (HeaderType ty)
deriving instance Ord (HeaderType ty)
deriving instance Show (HeaderType ty)
instance NFData (HeaderType ty) where
    rnf a = a `seq` ()

-- | Existential wrapper.
--
data SomeHeaderType where
    SomeHeaderType :: forall ty. HeaderType ty -> SomeHeaderType


-- | Singletons which makes it easier to work with 'HeaderType'
--
data SingHeaderType ty where
    SingHeaderTypeText :: SingHeaderType Text
    SingHeaderTypeInt  :: SingHeaderType Int

headerTypeSing :: HeaderType ty -> SingHeaderType ty
headerTypeSing = \case
    FileEncoding     -> SingHeaderTypeText
    FileFormat       -> SingHeaderTypeInt
    FileSorted       -> SingHeaderTypeInt
    OutputMode       -> SingHeaderTypeText
    KindDescription  -> SingHeaderTypeText
    KindSeparator    -> SingHeaderTypeText
    ProgramAuthor    -> SingHeaderTypeText
    ProgramName      -> SingHeaderTypeText
    ProgramUrl       -> SingHeaderTypeText
    ProgramVersion   -> SingHeaderTypeText

    ExtraDescription -> SingHeaderTypeText
    FieldDescription -> SingHeaderTypeText
    PseudoTag {}     -> SingHeaderTypeText
