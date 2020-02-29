{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.GhcTags ( plugin ) where

import           Control.Exception
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS
import           Data.IORef
import           Data.Map ( Map )
import qualified Data.Map as Map
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           GhcPlugins ( CommandLineOption
                            , Hsc
                            , HsParsedModule (..)
                            , ModSummary
                            , Plugin (..)
                            , liftIO
                            )
import           GhcPlugins hiding (occName, (<>))

import           Data.Foldable ( foldl' )
import           Data.Maybe    ( mapMaybe )
-- import qualified Data.ByteString as BS
import           Data.ByteString.Builder

-- Ghc imports
import           HsBinds      ( HsBindLR (..)
                              , PatSynBind (..)
                              , Sig (..)
                              )
import           HsDecls      ( ConDecl (..)
                              , FamilyDecl (..)
                              , ForeignDecl (..)
                              , LHsDecl
                              , HsDecl (..)
                              , HsDataDefn (..)
                              , TyClDecl (..)
                              )
import           HsSyn        ( GhcPs
                              , HsModule (..)
                              )

import           Plugin.GhcTags.Parser


-- | IORef which is shared across various compilations
--
tagsIORef :: IORef (Maybe (Map TagName [Tag]))
tagsIORef = unsafePerformIO $ newIORef Nothing


plugin :: Plugin
plugin = GhcPlugins.defaultPlugin { parsedResultAction = ghcTagPlugin }

ghcTagPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
ghcTagPlugin options _modSummary hsParsedModule@HsParsedModule {hpm_module} =
    liftIO $ do
      mTagsMap <- readIORef tagsIORef
      (tagsMap :: TagsMap) <-
        case mTagsMap of

          Nothing -> do
            res <- (BS.readFile tagsFile >>= parseVimTagFile)
                     `catch` \(e :: IOError) -> return (Left $ show e)
            case res of
              Left err -> do
                putStrLn $ "GhcTags: error parsing \"" ++ tagsFile ++ "\": " ++ err
                return $ Map.empty
              Right tagList -> do
                return $ mkTagsMap tagList

          Just tagsMap -> return tagsMap

      let tagsMap', updatedTagsMap :: TagsMap
          tagsMap' =
              mkTagsMap
            $ mapMaybe ghcTagToTag
            $ generateTagsForModule
            $ hpm_module

          updatedTagsMap = Map.union tagsMap' tagsMap

      -- update 'tagsIORef', make sure that `updateTagsMap` is evaluated.
      writeIORef tagsIORef (updatedTagsMap `seq` Just updatedTagsMap)

      -- update tags file
      withFile tagsFile AppendMode $ \fhandle ->
        BS.hPutBuilder fhandle
          $ foldMap formatVimTag
          $ concat
          $ Map.elems updatedTagsMap
      pure $ hsParsedModule
  where
    tagsFile :: FilePath
    tagsFile = case options of
      []    -> "tags"
      a : _ -> a

-- | We can read names from using fields of type 'GHC.Hs.Extensions.IdP' (a tpye
-- family) which for @'Parsed@ resolved to 'RdrName'
--
data GhcTag = GhcTag {
    tagSrcSpan  :: !SrcSpan
  , tagTag      :: !FastString
  }

type GhcTags = [GhcTag]

mkGhcTag :: Located RdrName -- Located (IdP GhcPs)
         -> GhcTag
mkGhcTag (L tagSrcSpan rdrName) =
    case rdrName of
      Unqual occName ->
        GhcTag { tagTag = occNameFS occName
               , tagSrcSpan
               }

      Qual _ occName ->
        GhcTag { tagTag = occNameFS occName
               , tagSrcSpan
               }

      -- Orig is the only one we are interested in
      Orig _ occName ->
        GhcTag { tagTag = occNameFS occName
               , tagSrcSpan
               }

      Exact name                   -> 
        GhcTag { tagTag = occNameFS $ nameOccName name
               , tagSrcSpan
               }

-- | generate tags for a module
--
generateTagsForModule :: Located (HsModule GhcPs)
                      -> GhcTags
generateTagsForModule (L _ HsModule { hsmodDecls }) = 
    reverse $ foldl' go [] hsmodDecls
  where
    go :: GhcTags -> LHsDecl GhcPs -> GhcTags
    go tags (L _ hsDecl) = case hsDecl of
      -- type or class declaration
      TyClD _ tyClDecl ->
        case tyClDecl of
          FamDecl { tcdFam } ->
            case mkFamilyDeclTags tcdFam of
              Just tag -> tag : tags
              Nothing  ->       tags

          SynDecl { tcdLName } ->
            mkGhcTag tcdLName : tags

          DataDecl { tcdLName, tcdDataDefn } -> 
            case tcdDataDefn of
              HsDataDefn { dd_cons } ->
                mkGhcTag tcdLName : ((mkConsTags . unLoc) `concatMap` dd_cons) ++ tags

              XHsDataDefn {} ->
                tags

          ClassDecl { tcdLName, tcdSigs, tcdMeths, tcdATs } ->
            -- class name
            mkGhcTag tcdLName
            -- class methods
            : (mkSigTags . unLoc) `concatMap` tcdSigs
            -- default methods
            ++ foldl' (\tags' hsBind -> mkHsBindLRTags (unLoc hsBind) ++ tags')
                     tags
                     tcdMeths
            -- associated types
            ++ (mkFamilyDeclTags . unLoc) `mapMaybe` tcdATs

          XTyClDecl {} -> tags

      -- TODO: instance declaration (type class & type family instances)
      -- here we can also scan for data & type family instances
      InstD {} -> tags

      -- deriveving declaration
      DerivD {} -> tags

      -- value declaration
      ValD _ hsBind  -> mkHsBindLRTags hsBind ++ tags

      -- signature declaration
      SigD _ sig -> mkSigTags sig ++ tags

      -- default declaration
      DefD {} -> tags

      -- foreign declaration
      ForD _ foreignDecl ->
        case foreignDecl of
          ForeignImport { fd_name } -> mkGhcTag fd_name : tags

          ForeignExport { fd_name } -> mkGhcTag fd_name : tags

          XForeignDecl {} -> tags

      WarningD {}   -> tags
      AnnD {}       -> tags

      -- TODO: Rules are named it would be nice to get them too
      RuleD {}      -> tags

      -- TODO: splices
      SpliceD {}    -> tags

      DocD {}       -> tags
      RoleAnnotD {} -> tags
      XHsDecl {}    -> tags

    -- tags of all constructors of a type
    mkConsTags :: ConDecl GhcPs -> GhcTags
    mkConsTags ConDeclGADT { con_names } = mkGhcTag `map` con_names
    mkConsTags ConDeclH98  { con_name  } = [mkGhcTag con_name]
    mkConsTags XConDecl    {}            = []

    mkHsBindLRTags :: HsBindLR GhcPs GhcPs -> GhcTags
    mkHsBindLRTags hsBind =
      case hsBind of
        FunBind { fun_id } -> [mkGhcTag fun_id]

        -- TODO
        -- This is useful fo generating tags for
        -- ````
        -- Just x = lhs
        -- ```
        PatBind {} -> []

        VarBind { var_id, var_rhs = L srcSpan _ } -> [mkGhcTag (L srcSpan var_id)]

        -- abstraction binding are only used after translaction
        AbsBinds {} -> []

        PatSynBind _ PSB { psb_id } -> [mkGhcTag psb_id]
        PatSynBind _ XPatSynBind {} -> []

        XHsBindsLR {} -> []

    mkSigTags :: Sig GhcPs -> GhcTags
    mkSigTags (TypeSig   _ lhs _)    = mkGhcTag `map` lhs
    mkSigTags (PatSynSig _ lhs _)    = mkGhcTag `map` lhs
    mkSigTags (ClassOpSig _ _ lhs _) = mkGhcTag `map` lhs
    mkSigTags IdSig {}               = []
    -- TODO: generate theses with additional info (fixity)
    mkSigTags FixSig {}              = []
    mkSigTags InlineSig {}           = []
    -- SPECIALISE pragmas
    mkSigTags SpecSig {}             = []
    mkSigTags SpecInstSig {}         = []
    -- MINIMAL pragma
    mkSigTags MinimalSig {}          = []
    -- SSC pragma
    mkSigTags SCCFunSig {}           = []
    -- COMPLETE pragma
    mkSigTags CompleteMatchSig {}    = []
    mkSigTags XSig {}                = []

    mkFamilyDeclTags :: FamilyDecl GhcPs -> Maybe GhcTag
    mkFamilyDeclTags FamilyDecl { fdLName } = Just $ mkGhcTag fdLName
    mkFamilyDeclTags XFamilyDecl {}         = Nothing


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { tagSrcSpan, tagTag } =
    case tagSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tag     = TagName (fs_bs tagTag)
                   , tagFile = fs_bs (srcSpanFile realSrcSpan)
                   , tagLine = srcSpanStartLine realSrcSpan
                   }

formatVimTag :: Tag -> Builder
formatVimTag Tag { tag, tagFile, tagLine } =
        byteString (getTagName tag)
    <> charUtf8 '\t'
    <> byteString tagFile
    <> charUtf8 '\t'
    <> intDec tagLine
    <> charUtf8 '\n'
