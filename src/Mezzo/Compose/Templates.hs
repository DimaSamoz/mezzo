{-# LANGUAGE TypeInType, TemplateHaskell, ExplicitForAll, ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Templates
-- Description :  TH templates
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Template Haskell functions to generate music literals.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Templates
    ( pitchClassLits
    , accidentalLits
    , octaveLits
    , mkDurLits
    , mk32ndLits
    , mkPitchLits
    , mkPitchSpecs
    , scaleDegreeLits
    , modeLits
    , triTyLits
    , sevTyLits
    , invLits
    , mkTriConvs
    , mkSevConvs
    , mkDoubledConvs
    ) where

import Mezzo.Model
import Mezzo.Compose.Types
import Mezzo.Compose.Builder

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)
import Data.Char
import Control.Monad (join)


-------------------------------------------------------------------------------
-- Literal templates
-------------------------------------------------------------------------------

-- | Generate pitch class literal declarations.
pitchClassLits :: DecsQ
pitchClassLits = genLitDecs pcFormatter "PC" ''PitchClass

-- | Generate accidental literal declarations.
accidentalLits :: DecsQ
accidentalLits = genLitDecs accFormatter "Acc" ''Accidental

-- | Generate octave literal declarations.
octaveLits :: DecsQ
octaveLits = genLitDecs octFormatter "Oct" ''OctaveNum

-- | Generate scale degree literal declarations.
scaleDegreeLits :: DecsQ
scaleDegreeLits = genLitDecs scaDegFormatter "ScaDeg" ''ScaleDegree

-- | Generate mode literal declarations.
modeLits :: DecsQ
modeLits = genLitDecs modeFormatter "Mod" ''Mode -- Might want to extend modes later

-- | Generate triad type literal declarations.
triTyLits :: DecsQ
triTyLits = genLitDecs choTyFormatter "TriType" ''TriadType

-- | Generate seventh type literal declarations.
sevTyLits :: DecsQ
sevTyLits = do
    dcs <- filter (\n -> nameBase n /= "Doubled") <$> getDataCons ''SeventhType
    join <$> traverse (mkSingLit choTyFormatter "SevType") dcs

invLits :: DecsQ
invLits = genLitDecs invFormatter "Inv" ''Inversion

-------------------------------------------------------------------------------
-- Templates and generators
-------------------------------------------------------------------------------

-- | Make a literal value declaration for a singleton with the given format string,
-- singleton name and data constructor.
mkSingLit :: Formatter -> String -> Name -> DecsQ
mkSingLit format sing dataName = do
        tySig <- sigD valName $ appT pcTyCon ty
        dec <- [d| $(valPat) = $(pcDataCon) |]
        return $ tySig : dec
    where
        valName = mkName (format dataName)
        valPat = varP valName
        pcTyCon = conT $ mkName sing
        ty = promotedT dataName
        pcDataCon = conE $ mkName sing

-- | Generate concrete pitch literals for each pitch class, accidental and octave.
mkPitchLits :: DecsQ
mkPitchLits = do
    pcNames <- getDataCons ''PitchClass
    accNames <- getDataCons ''Accidental
    octNames <- getDataCons ''OctaveNum
    let declareVal pc acc oct = do  -- Generates a type signature and value declaration for the specified pitch
            let pcStr = pcFormatter pc
                accStr = shortAccFormatter acc
                octStr = shortOctFormatter oct
                valName = mkName $ pcStr ++ accStr ++ octStr
            tySig <- sigD valName $ [t| Pit (Pitch $(conT pc) $(conT acc) $(conT oct)) |]
            dec <- [d| $(varP valName) = pitch $(varE $ mkName pcStr) $(varE $ mkName (accFormatter acc)) $(varE $ mkName (octFormatter oct)) |]
            return $ tySig : dec
    join <$> sequence (declareVal <$> pcNames <*> accNames <*> octNames)    -- Every combination of PCs, Accs and Octs

mkDurLits :: Name -> DecsQ
mkDurLits name = do
    let litName = mkName $ durLitFormatter name
        litName' = mkName $ (durLitFormatter name ++ "\'")
    literal <- do
            tySig1 <- sigD litName $ [t| Dur $(conT name) |]
            dec1 <- [d| $(varP litName) = Dur |]
            tySig1' <- sigD litName' $ [t| Dur (Dot $(conT name)) |]
            dec1' <- [d| $(varP litName') = Dur |]
            return $ tySig1 : dec1 ++ tySig1' : dec1'
    noteTerm <- do
            let valName = mkName $ (durLitFormatter name) !! 1 : "n"
            tySig2 <- sigD valName $ [t| forall r. IntRep r => RootT r $(conT name) |]
            dec2 <- [d| $(varP valName) = \p -> Note p $(varE litName) |]
            let valName' = mkName $ (durLitFormatter name) !! 1 : "n\'"
            tySig2' <- sigD valName' $ [t| forall r. IntRep r => RootT r (Dot $(conT name)) |]
            dec2' <- [d| $(varP valName') = \p -> Note p $(varE litName') |]
            return $ tySig2 : dec2 ++ tySig2' : dec2'
    restTerm <- do
            let valName = mkName $ (durLitFormatter name) !! 1 : "r"
            tySig2 <- sigD valName $ [t| RestT $(conT name) |]
            dec2 <- [d| $(varP valName) = const (Rest $(varE litName)) |]
            let valName' = mkName $ (durLitFormatter name) !! 1 : "r\'"
            tySig2' <- sigD valName' $ [t| RestT (Dot $(conT name)) |]
            dec2' <- [d| $(varP valName') = const (Rest $(varE litName')) |]
            return $ tySig2 : dec2 ++ tySig2' : dec2'
    chordTerm <- do
            let valName = mkName $ (durLitFormatter name) !! 1 : "c"
            tySig2 <- sigD valName $ [t| forall n r. (Primitive n, IntListRep r) => ChorT (r :: ChordType n) $(conT name) |]
            dec2 <- [d| $(varP valName) = \c -> Chord c $(varE litName) |]
            let valName' = mkName $ (durLitFormatter name) !! 1 : "c\'"
            tySig2' <- sigD valName' $ [t| forall n r. (Primitive n, IntListRep r) => ChorT (r :: ChordType n) (Dot $(conT name)) |]
            dec2' <- [d| $(varP valName') = \c -> Chord c $(varE litName') |]
            return $ tySig2 : dec2 ++ tySig2' : dec2'
    return $ literal ++ noteTerm ++ restTerm ++ chordTerm

mk32ndLits :: DecsQ -- Don't want to make dotted literals for thirty second notes.
mk32ndLits = do
    let litName = mkName $ "_th"
    literal <- do
            tySig1 <- sigD litName $ [t| Dur $(conT ''ThirtySecond) |]
            dec1 <- [d| $(varP litName) = Dur |]
            return $ tySig1 : dec1
    noteTerm <- do
            let valName = mkName $ "tn"
            tySig2 <- sigD valName $ [t| forall r. IntRep r => RootT r $(conT ''ThirtySecond) |]
            dec2 <- [d| $(varP valName) = \p -> Note p $(varE litName) |]
            return $ tySig2 : dec2
    restTerm <- do
            let valName = mkName $ "tr"
            tySig2 <- sigD valName $ [t| RestT $(conT ''ThirtySecond) |]
            dec2 <- [d| $(varP valName) = const (Rest $(varE litName)) |]
            return $ tySig2 : dec2
    chordTerm <- do
            let valName = mkName $ "tc"
            tySig2 <- sigD valName $ [t| forall n r. (Primitive n, IntListRep r) => ChorT (r :: ChordType n) $(conT ''ThirtySecond) |]
            dec2 <- [d| $(varP valName) = \c -> Chord c $(varE litName)  |]
            return $ tySig2 : dec2
    return $ literal ++ noteTerm ++ restTerm ++ chordTerm

-- | Generate pitch root specifiers for earch pitch class, accidental and octave.
-- These allow for combinatorial input with CPS-style durations and modifiers.
mkPitchSpecs :: DecsQ
mkPitchSpecs = do
    pcNames <- getDataCons ''PitchClass
    accNames <- getDataCons ''Accidental
    octNames <- getDataCons ''OctaveNum
    let declareVal pc acc oct = do
            let pcStr = tail $ pcFormatter pc
                accStr = shorterAccFormatter acc
                octStr = shortOctFormatter oct
                valName = mkName $ pcStr ++ accStr ++ octStr
            tySig <- sigD valName $
                [t| RootS (PitchRoot (Pitch $(conT pc) $(conT acc) $(conT oct) )) |]
            dec <- [d| $(varP valName) = spec Root |]
            return $ tySig : dec
    join <$> sequence (declareVal <$> pcNames <*> accNames <*> octNames)

-- | Generate converters from roots to triads, for each triad type.
mkTriConvs :: DecsQ
mkTriConvs = do
    triTyNames <- getDataCons ''TriadType
    let declareFun choTy = do
            let choStr = tail (choTyFormatter choTy)
                valName1 = mkName $ choStr ++ "'"
                valName2 = mkName $ choStr
            tySig1 <- sigD valName1 $
                [t| forall r i. ChorC' Triad r $(conT choTy) i |]
            dec1 <- [d| $(varP valName1) = \i -> constConv Cho |]
            tySig2 <- sigD valName2 $
                [t| forall r. ChorC Triad r $(conT choTy) |]
            dec2 <- [d| $(varP valName2) = constConv Cho |]
            return $ (tySig1 : dec1) ++ (tySig2 : dec2)
    join <$> traverse declareFun triTyNames

-- | Generate converters from roots to seventh chords, for each seventh type.
mkSevConvs :: DecsQ
mkSevConvs = do
    sevTyNames <- filter (\n -> nameBase n /= "Doubled") <$> getDataCons ''SeventhType
    let declareFun choTy = do
            let choStr = tail (choTyFormatter choTy)
                valName1 = mkName $ choStr ++ "'"
                valName2 = mkName $ choStr
            tySig1 <- sigD valName1 $
                [t| forall r i. ChorC' SeventhChord r $(conT choTy) i |]
            dec1 <- [d| $(varP valName1) = \i -> constConv Cho |]
            tySig2 <- sigD valName2 $
                [t| forall r. ChorC SeventhChord r $(conT choTy) |]
            dec2 <- [d| $(varP valName2) = constConv Cho |]
            return $ (tySig1 : dec1) ++ (tySig2 : dec2)
    join <$> traverse declareFun sevTyNames

-- | Generate converters from roots to doubled seventh chords, for each triad type.
mkDoubledConvs :: DecsQ
mkDoubledConvs = do
    triTyNames <- getDataCons ''TriadType
    let declareFun choTy = do
            let choStr = tail (choTyFormatter choTy)
                valName1 = mkName $ choStr ++ "D'"
                valName2 = mkName $ choStr ++ "D"
            tySig1 <- sigD valName1 $
                [t| forall r i. ChorC' SeventhChord r (Doubled $(conT choTy)) i |]
            dec1 <- [d| $(varP valName1) = \i -> constConv Cho |]
            tySig2 <- sigD valName2 $
                [t| forall r. ChorC SeventhChord r (Doubled $(conT choTy)) |]
            dec2 <- [d| $(varP valName2) = constConv Cho |]
            return $ (tySig1 : dec1) ++ (tySig2 : dec2)
    join <$> traverse declareFun triTyNames




-------------------------------------------------------------------------------
-- Formatters
-------------------------------------------------------------------------------

-- | Type synonym for a formatter, that specifies how a data constructor name is
-- diplayed as a literal value.
type Formatter = Name -> String

-- | 'PitchClass' formatter.
pcFormatter :: Formatter
pcFormatter pc = '_' : map toLower (nameBase pc)

-- | 'Accidental' formatter.
accFormatter :: Formatter
accFormatter = ('_' :) . map toLower . take 2 . nameBase

-- | 'OctaveNum' formatter.
octFormatter :: Formatter
octFormatter oct = "_o" ++ drop 3 (nameBase oct)

-- | One letter accidental with explicit 'Naturals'.
shortAccFormatter :: Formatter
shortAccFormatter (accFormatter -> "_fl") = "f"
shortAccFormatter (accFormatter -> name) = [name !! 1]

-- | One letter accidental with implicit 'Naturals'.
shorterAccFormatter :: Formatter
shorterAccFormatter (shortAccFormatter -> "n") = ""
shorterAccFormatter (shortAccFormatter -> name) = name

-- | Symbolic suffix format for octaves.
shortOctFormatter :: Formatter
shortOctFormatter name = case nameBase name of
    "Oct_1" -> "_5"
    "Oct0"  -> "_4"
    "Oct1"  -> "_3"
    "Oct2"  -> "__"
    "Oct3"  -> "_"
    "Oct4"  -> ""
    "Oct5"  -> "'"
    "Oct6"  -> "''"
    "Oct7"  -> "'3"
    "Oct8"  -> "'4"

-- | Formatter for duration literals.
durLitFormatter :: Formatter
durLitFormatter = ('_' :) . map toLower . take 2 . nameBase

-- | Formatter for pitch literals.
pitchLitFormatter :: Name -> Name -> Name -> String
pitchLitFormatter pc acc oct = pcFormatter pc ++ shortAccFormatter acc ++ shortOctFormatter oct

-- | Formatter for scale degrees.
scaDegFormatter :: Formatter
scaDegFormatter name = "_" ++ map toLower (nameBase name)

-- | Formatter for key modes.
modeFormatter :: Formatter
modeFormatter (nameBase -> name) = map toLower (take 3 name) ++ dropWhile isLower (tail name)

-- | Formatter for chords types.
choTyFormatter :: Formatter
choTyFormatter n = case nameBase n of
    "MajTriad"       -> "_maj"
    "MinTriad"       -> "_min"
    "AugTriad"       -> "_aug"
    "DimTriad"       -> "_dim"
    "MajSeventh"     -> "_maj7"
    "MajMinSeventh"  -> "_sev"
    "MinSeventh"     -> "_min7"
    "HalfDimSeventh" -> "_hdim7"
    "DimSeventh"     -> "_dim7"

-- | Formatter for inversions.
invFormatter :: Formatter
invFormatter (nameBase -> name) = "i" ++ [last name]

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Get the data constructors of a type.
getDataCons :: Name -> Q [Name]
getDataCons tyName = do
    TyConI (DataD _ _ _ _ dcs _) <- reify tyName
    return $ reverse $ map (\(NormalC pc _) -> pc) dcs

-- | Map a function generating declarations from a data constructor to all
-- data constructors of a type.
mapToDataCons :: (Name -> DecsQ) -> Name -> DecsQ
mapToDataCons f tyName = do
    dcNames <- getDataCons tyName
    join <$> traverse f dcNames

genLitDecs :: Formatter -> String -> Name -> DecsQ
genLitDecs format singName name = mapToDataCons (mkSingLit format singName) name
