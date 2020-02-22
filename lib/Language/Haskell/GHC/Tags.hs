{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.GHC.Tags where

-- Ghc imports
import           DynFlags     ( DynFlags )
import           GHC          ( runGhc )
import           FastString   ( FastString
                              , fsLit
                              )
import           GhcMonad     ( reifyGhc
                              , getSessionDynFlags
                              )
import           HsSyn        ( GhcPs
                              , HsModule (..)
                              )
import           Lexer        ( P (unP)
                              , ParseResult (..)
                              , mkPState
                              )
import           Name         ( nameOccName )
import           OccName      ( OccName (..) )
import           Parser       ( parseModule )
import           RdrName      ( RdrName (..) )
import           SrcLoc       ( Located
                              , SrcLoc
                              , RealSrcLoc
                              , mkRealSrcLoc
                              )
import           StringBuffer ( StringBuffer )
import qualified StringBuffer

import qualified GHC.Paths

-- Ghc a ~ Session -> IO a
--
-- runGhc

-- | Parse a module.
--
parseGhcModule :: DynFlags
               -> StringBuffer
               -> RealSrcLoc
               -- can be created with 'mkRealSrcLoc'
               -> ParseResult (Located (HsModule GhcPs))
parseGhcModule dynFlags stringBuffer realLocSrc =
    unP parseModule (mkPState dynFlags stringBuffer realLocSrc)


-- | This is only first approximation.  We need to get all the flags from cabal
-- files.
--
-- TODO: 'GHC.Paths.libdir' should be dynamically configurable, not given at
-- compile time
--
parseGhcModuleIO :: FilePath
                 -> IO (ParseResult (Located (HsModule GhcPs)))
parseGhcModuleIO modulePath =
    runGhc (Just GHC.Paths.libdir) $ do
      stringBuffer <- reifyGhc $ \_ -> StringBuffer.hGetStringBuffer modulePath
      dynFlags <- getSessionDynFlags
      let realSrcLoc = mkRealSrcLoc (fsLit modulePath)
                                    (StringBuffer.cur stringBuffer)
                                    (StringBuffer.len stringBuffer)
      return $ parseGhcModule dynFlags stringBuffer realSrcLoc


-- | We can read names from using fields of type 'GHC.Hs.Extensions.IdP' (a tpye
-- family) which for @'Parsed@ resolved to 'RdrName'
--
data GhcTag = GhcTag {
    tagFilePath :: FilePath
  , tagSrcLoc   :: SrcLoc
  , tagTag      :: FastString
  }

mkGhcTag :: FilePath
         -> SrcLoc
         -> RdrName
         -> GhcTag
mkGhcTag tagFilePath tagSrcLoc rdrName =
    case rdrName of
      Unqual occName ->
        GhcTag { tagTag = occNameFS occName
               , tagSrcLoc
               , tagFilePath
               }

      Qual _ occName ->
        GhcTag { tagTag = occNameFS occName
               , tagSrcLoc
               , tagFilePath
               }

      -- Orig is the only one we are interested in
      Orig _ occName ->
        GhcTag { tagTag = occNameFS occName
               , tagSrcLoc
               , tagFilePath
               }

      Exact name                   -> 
        GhcTag { tagTag = occNameFS $ nameOccName name
               , tagSrcLoc
               , tagFilePath
               }
