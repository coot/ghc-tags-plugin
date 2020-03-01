{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.GhcTags ( plugin ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS
import           Data.IORef
import           Data.List (sort)
import qualified Data.Map as Map
-- import           Data.Foldable (traverse_)
import           System.IO
import           System.IO.Error  (tryIOError)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Directory

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
import           HsDecls      ( ClsInstDecl (..)
                              , ConDecl (..)
                              , DataFamInstDecl (..)
                              , FamEqn (..)
                              , FamilyDecl (..)
                              , ForeignDecl (..)
                              , LHsDecl
                              , HsDecl (..)
                              , HsDataDefn (..)
                              , InstDecl (..)
                              , TyClDecl (..)
                              , TyFamInstDecl (..)
                              )
import           HsSyn        ( GhcPs
                              , HsModule (..)
                              )
import           HsTypes      ( HsImplicitBndrs (..)
                              , HsType (..)
                              , LHsType
                              )

import           Plugin.GhcTags.Parser


-- | IORef which is shared across various compilations
--
tagsIORef :: IORef (Maybe TagsMap)
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
            a <- doesFileExist tagsFile
            res <-
              if a
                then do
                  mbytes <- tryIOError (BS.readFile tagsFile)
                  case mbytes of
                    Left err    -> do
                      putStrLn $ "GhcTags: error reading \"" ++ tagsFile ++ "\": " ++ (show err)
                      return $ Right []
                    Right bytes ->
                      parseVimTagFile bytes
                else return $ Right []
            case res of
              Left err -> do
                putStrLn $ "GhcTags: error reading or parsing \"" ++ tagsFile ++ "\": " ++ err
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

          updatedTagsMap = tagsMap' `Map.union` tagsMap

      {-
      putStrLn $ "tags found"
      traverse_ print
        $ sortOn tagName
        $ concat
        $ Map.elems tagsMap'
      -}

      -- update 'tagsIORef', make sure that `updateTagsMap` is evaluated.
      -- TODO: this is not attomic, which will break when compiling multiple
      -- modules at the same time.  I think we need to use 'MVar' and
      -- 'takeMVar'.
      writeIORef tagsIORef (updatedTagsMap `seq` Just updatedTagsMap)

      -- update tags file
      withFile tagsFile WriteMode $ \fhandle ->
        BS.hPutBuilder fhandle
          $ foldMap formatVimTag
          $ sort
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
  deriving Show

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

          -- TODO: add 'tcdATDefs'
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
      InstD _ instDecl ->
        case instDecl of
          ClsInstD { cid_inst } ->
            case cid_inst of
              XClsInstDecl {} -> tags

              ClsInstDecl { cid_poly_ty, cid_tyfam_insts, cid_datafam_insts } ->
                  case cid_poly_ty of
                    XHsImplicitBndrs {} ->
                      tyFamTags ++ dataFamTags ++ tags

                    -- TODO: @hsbib_body :: LHsType GhcPs@
                    HsIB { hsib_body } ->
                      case mkLHsTypeTag hsib_body of
                        Nothing  -> tyFamTags ++ dataFamTags ++ tags
                        Just tag -> tag : tyFamTags ++ dataFamTags ++ tags
                where
                  dataFamTags = (mkDataFamInstDeclTag . unLoc) `concatMap` cid_datafam_insts
                  tyFamTags   = (mkTyFamInstDeclTag   . unLoc) `mapMaybe`  cid_tyfam_insts


          DataFamInstD { dfid_inst } ->
            mkDataFamInstDeclTag  dfid_inst ++ tags

          TyFamInstD { tfid_inst } ->
            case mkTyFamInstDeclTag tfid_inst of
              Nothing  -> tags
              Just tag -> tag : tags

          XInstDecl {} -> tags

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

    -- used to generate tag of an instance declaration
    mkLHsTypeTag :: LHsType GhcPs -> Maybe GhcTag
    mkLHsTypeTag (L _ hsType) =
      case hsType of
        HsForAllTy {hst_body} -> mkLHsTypeTag hst_body
        
        HsQualTy {hst_body}   -> mkLHsTypeTag hst_body

        HsTyVar _ _ a -> Just $ mkGhcTag a

        HsAppTy _ a _         -> mkLHsTypeTag a
        HsOpTy _ _ a _        -> Just $ mkGhcTag a
        HsKindSig _ a _       -> mkLHsTypeTag a

        _                     -> Nothing

    -- todo: type constructors
    mkDataFamInstDeclTag :: DataFamInstDecl GhcPs -> GhcTags
    mkDataFamInstDeclTag DataFamInstDecl { dfid_eqn } =
      case dfid_eqn of
        XHsImplicitBndrs {} -> []

        HsIB { hsib_body = FamEqn { feqn_tycon, feqn_rhs } } ->
          case feqn_rhs of
            HsDataDefn { dd_cons } ->
              mkGhcTag feqn_tycon : (mkConsTags . unLoc) `concatMap` dd_cons

        HsIB { hsib_body = XFamEqn {} } -> []

    mkTyFamInstDeclTag :: TyFamInstDecl GhcPs -> Maybe GhcTag
    mkTyFamInstDeclTag TyFamInstDecl { tfid_eqn } =
      case tfid_eqn of
        XHsImplicitBndrs {} -> Nothing

        -- TODO: should we check @feqn_rhs :: LHsType GhcPs@ as well?
        HsIB { hsib_body = FamEqn { feqn_tycon } } -> Just $ mkGhcTag feqn_tycon

        HsIB { hsib_body = XFamEqn {} } -> Nothing


ghcTagToTag :: GhcTag -> Maybe Tag
ghcTagToTag GhcTag { tagSrcSpan, tagTag } =
    case tagSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan ->
        Just $ Tag { tagName = TagName (fs_bs tagTag)
                   , tagFile = TagFile (fs_bs (srcSpanFile realSrcSpan))
                   , tagLine = srcSpanStartLine realSrcSpan
                   }

formatVimTag :: Tag -> Builder
formatVimTag Tag { tagName, tagFile, tagLine } =
        byteString (getTagName tagName)
    <> charUtf8 '\t'
    <> byteString (getTagFile tagFile)
    <> charUtf8 '\t'
    <> intDec tagLine
    <> charUtf8 '\n'
