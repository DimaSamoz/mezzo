{-# LANGUAGE CPP, TupleSections, StandaloneDeriving #-}

module Mezzo.Model.Plugin
  ( plugin )
where

import Mezzo.Model.Types (PitchClass (..), Accidental (..), OctaveNum (..))

-- external
import Control.Arrow       ((***))
import Data.List           (partition)
import Data.Maybe          (mapMaybe)
import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)

-- GHC API
import FastString (fsLit, unpackFS)
import Module     (mkModuleName)
import OccName    (mkTcOcc, occNameFS, occName)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon, tcLookupDataCon)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..),
                   ctEvidence, ctEvPred)
import TyCon      (TyCon, tyConDataCons, )
import DataCon    (promoteDataCon, dataConName)
import Type       (EqRel (NomEq), PredTree (EqPred),
                   classifyPredType)
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (Type (..), TyLit (..))
#else
import TypeRep    (Type (..), TyLit (..))
#endif

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \_ -> Just gcdPlugin }


gcdPlugin :: TcPlugin
gcdPlugin =
  TcPlugin { tcPluginInit  = lookupPitchTyCon
           , tcPluginSolve = resolvePitchEquality
           , tcPluginStop  = \_ -> return ()
           }

lookupPitchTyCon :: TcPluginM TyCon
lookupPitchTyCon = do
    md <- lookupModule typesModule mezzoPackage
    pitchTypeCon <- look md "PitchType"
    return $ getDataCon pitchTypeCon "Pitch"
    where
        look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
        typesModule = mkModuleName "Mezzo.Model.Types"
        mezzoPackage = fsLit "mezzo"
        getDataCon u s = case [ dc | dc <- tyConDataCons u, occNameFS (occName (dataConName dc)) == fsLit s ] of
                       [d] -> promoteDataCon d
                       _   -> error $ "Missing data constructor: " ++ s

resolvePitchEquality :: TyCon
                -> [Ct] -- ^ [G]iven constraints
                -> [Ct] -- ^ [D]erived constraints
                -> [Ct] -- ^ [W]anted constraints
                -> TcPluginM TcPluginResult
resolvePitchEquality _ _ _ [] = return (TcPluginOk [] [])
resolvePitchEquality pitchTc _ _ wanteds = return $! case failed of
    [] -> TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []
    f -> TcPluginContradiction f
    where
        relevant :: [(Ct, (PitchRep, PitchRep))]
        relevant = mapMaybe (toEnhEquality pitchTc) wanteds

        solved, failed :: [Ct]
        (solved, failed) = (map fst *** map fst)
                         $ partition (enhEqual . snd) relevant

deriving instance Eq PitchClass
deriving instance Eq Accidental
deriving instance Eq OctaveNum
deriving instance Read PitchClass
deriving instance Read Accidental
deriving instance Read OctaveNum
deriving instance Enum PitchClass
deriving instance Enum OctaveNum

data PitchRep = PR PitchClass Accidental OctaveNum

enhEqual :: (PitchRep, PitchRep) -> Bool
enhEqual (PR C Flat o1, PR B Natural o2)  = o1 == succ o2
enhEqual (PR C Natural o1, PR B Sharp o2) = o1 == succ o2
enhEqual (PR E Natural o1, PR F Flat o2)  = o1 == o2
enhEqual (PR E Sharp o1, PR F Natural o2) = o1 == o2
enhEqual (PR F Flat o1, PR E Natural o2)  = o1 == o2
enhEqual (PR F Natural o1, PR E Sharp o2) = o1 == o2
enhEqual (PR B Natural o1, PR C Flat o2)  = o1 == pred o2
enhEqual (PR B Sharp o1, PR C Natural o2) = o1 == pred o2
enhEqual (PR c1 Sharp o1, PR c2 Flat o2)  = c1 == pred c2 && o1 == o2
enhEqual (PR c1 Flat o1, PR c2 Sharp o2)  = c1 == succ c2 && o1 == o2
enhEqual (PR c1 a1 o1, PR c2 a2 o2)       = (c1 == c2) && (a1 == a2) && (o1 == o2)

toEnhEquality :: TyCon -> Ct -> Maybe (Ct, (PitchRep, PitchRep))
toEnhEquality pitchTc ct =
    case classifyPredType $ ctEvPred $ ctEvidence ct of
        EqPred NomEq t1 t2
            -> (ct,) <$> ((,) <$> getPitchRep pitchTc t1
                              <*> getPitchRep pitchTc t2)
        _   -> Nothing

getPitchRep :: TyCon -> Type -> Maybe PitchRep
getPitchRep pitchTc = go
    where
        go (TyConApp tc [pc, acc, oct])
            | tc == pitchTc = Just (PR (readPc pc) (readAcc acc) (readOct oct))
        go _ = Nothing

        readPc :: Type -> PitchClass
        readPc (LitTy (StrTyLit s)) = read (unpackFS s)
        readAcc :: Type -> Accidental
        readAcc (LitTy (StrTyLit s)) = read (unpackFS s)
        readOct :: Type -> OctaveNum
        readOct (LitTy (StrTyLit s)) = read (unpackFS s)

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "mezzo" t1 t2)
    _                  -> Nothing
