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
    , mkPitchLits
    , mkPitchCombs
    , scaleDegreeLits
    , modeLits
    , triTyLits
    , sevTyLits
    , invLits
    , mkTriTyCombs
    , mkSevTyCombs
    , mkDoubledTyCombs
    ) where

import Mezzo.Model
import Mezzo.Compose.Types
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
                accStr = if accFormatter acc == "fl" then "b" else [head $ accFormatter acc]
                octStr = shortOctFormatter oct
                valName = mkName $ pcStr ++ accStr ++ octStr
            tySig <- sigD valName $ [t| Pit (Pitch $(conT pc) $(conT acc) $(conT oct)) |]
            dec <- [d| $(varP valName) = pitch $(varE $ mkName pcStr) $(varE $ mkName (accFormatter acc)) $(varE $ mkName (octFormatter oct)) |]
            return $ tySig : dec
    join <$> sequence (declareVal <$> pcNames <*> accNames <*> octNames)    -- Every combination of PCs, Accs and Octs

-- | Generate pitch combinators for earch pitch class, accidental and octave.
-- These allow for combinatorial input with CPS-style durations and modifiers.
mkPitchCombs :: DecsQ
mkPitchCombs = do
    pcNames <- getDataCons ''PitchClass
    accNames <- getDataCons ''Accidental
    octNames <- getDataCons ''OctaveNum
    let declareVal pc acc oct = do
            let pcStr = tail $ pcFormatter pc
                accStr = shorterAccFormatter acc
                octStr = shortOctFormatter oct
                valName = mkName $ pcStr ++ accStr ++ octStr
                pitchLitName = mkName $ pitchLitFormatter pc acc oct
            tySig <- sigD valName $
                [t| forall m. (Pit (Pitch $(conT pc) $(conT acc) $(conT oct)) -> m) -> m |]
            dec <- [d| $(varP valName) = \ dur -> dur $(varE pitchLitName) |]
            return $ tySig : dec
    join <$> sequence (declareVal <$> pcNames <*> accNames <*> octNames)

mkTriTyCombs :: DecsQ
mkTriTyCombs = do
    triTyNames <- getDataCons ''TriadType
    let declareFun choTy = do
            let choStr = tail (choTyFormatter choTy)
                valName1 = mkName $ choStr ++ "_"
                valName2 = mkName $ choStr
                litName = mkName $ choTyFormatter choTy
            tySig1 <- sigD valName1 $
                [t| forall p i d. Pit p -> Inv i -> DurC p d -> Music (FromChord (Triad (PitchRoot p) $(conT choTy) i) d) |]
            dec1 <- [d| $(varP valName1) = \ p i d -> Chord (triad (rootP p) $(varE litName) i) (getDur p d) |]
            tySig2 <- sigD valName2 $
                [t| forall p d. Pit p -> DurC p d -> Music (FromChord (Triad (PitchRoot p) $(conT choTy) Inv0) d) |]
            dec2 <- [d| $(varP valName2) = \ p d -> Chord (triad (rootP p) $(varE litName) i0) (getDur p d) |]
            return $ (tySig1 : dec1) ++ (tySig2 : dec2)
    join <$> traverse declareFun triTyNames

mkSevTyCombs :: DecsQ
mkSevTyCombs = do
    sevTyNames <- filter (\n -> nameBase n /= "Doubled") <$> getDataCons ''SeventhType
    let declareFun choTy = do
            let choStr = tail (choTyFormatter choTy)
                valName1 = mkName $ choStr ++ "_"
                valName2 = mkName $ choStr
                litName = mkName $ choTyFormatter choTy
            tySig1 <- sigD valName1 $
                [t| forall p i d. Pit p -> Inv i -> DurC p d -> Music (FromChord (SeventhChord (PitchRoot p) $(conT choTy) i) d) |]
            dec1 <- [d| $(varP valName1) = \ p i d -> Chord (seventh (rootP p) $(varE litName) i) (getDur p d) |]
            tySig2 <- sigD valName2 $
                [t| forall p d. Pit p -> DurC p d -> Music (FromChord (SeventhChord (PitchRoot p) $(conT choTy) Inv0) d) |]
            dec2 <- [d| $(varP valName2) = \ p d -> Chord (seventh (rootP p) $(varE litName) i0) (getDur p d) |]
            return $ (tySig1 : dec1) ++ (tySig2 : dec2)
    join <$> traverse declareFun sevTyNames

mkDoubledTyCombs :: DecsQ
mkDoubledTyCombs = do
    triTyNames <- getDataCons ''TriadType
    let declareFun choTy = do
            let choStr = tail (choTyFormatter choTy)
                valName1 = mkName $ choStr ++ "D_"
                valName2 = mkName $ choStr ++ "D"
                litName = mkName $ choTyFormatter choTy
            tySig1 <- sigD valName1 $
                [t| forall p i d. Pit p -> Inv i -> DurC p d -> Music (FromChord (SeventhChord (PitchRoot p) (Doubled $(conT choTy)) i) d) |]
            dec1 <- [d| $(varP valName1) = \ p i d -> Chord (seventh (rootP p) (_dbl $(varE litName)) i) (getDur p d) |]
            tySig2 <- sigD valName2 $
                [t| forall p d. Pit p -> DurC p d -> Music (FromChord (SeventhChord (PitchRoot p) (Doubled $(conT choTy)) Inv0) d) |]
            dec2 <- [d| $(varP valName2) = \ p d -> Chord (seventh (rootP p) (_dbl $(varE litName)) i0) (getDur p d) |]
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
accFormatter = map toLower . take 2 . nameBase

-- | 'OctaveNum' formatter.
octFormatter :: Formatter
octFormatter oct = 'o' : drop 3 (nameBase oct)

-- | One letter accidental with explicit 'Naturals'.
shortAccFormatter :: Formatter
shortAccFormatter (accFormatter -> "fl") = "b"
shortAccFormatter (accFormatter -> name) = [head name]

-- | One letter accidental with implicit 'Naturals'.
shorterAccFormatter :: Formatter
shorterAccFormatter (shortAccFormatter -> "n") = ""
shorterAccFormatter (shortAccFormatter -> name) = name

-- | Symbolic suffix format for octaves.
shortOctFormatter :: Formatter
shortOctFormatter name = case nameBase name of
    "Oct_1" -> "____"
    "Oct0"  -> "___"
    "Oct1"  -> "__"
    "Oct2"  -> "_"
    "Oct3"  -> ""
    "Oct4"  -> "'"
    "Oct5"  -> "''"
    "Oct6"  -> "'''"
    "Oct7"  -> "''''"
    "Oct8"  -> "'''''"

-- | Formatter for pitch literals.
pitchLitFormatter :: Name -> Name -> Name -> String
pitchLitFormatter pc acc oct = pcFormatter pc ++ shortAccFormatter acc ++ shortOctFormatter oct

-- | Formatter for scale degrees.
scaDegFormatter :: Formatter
scaDegFormatter = map toLower . nameBase

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
