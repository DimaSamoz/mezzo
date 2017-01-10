{-# LANGUAGE TypeInType, TemplateHaskell, ExplicitForAll #-}

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
    ) where

import Mezzo.Model
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)
import Data.Char
import Control.Monad (join)


-------------------------------------------------------------------------------
-- Literal templates
-------------------------------------------------------------------------------

-- | Generate pitch class literal declarations.
pitchClassLits :: DecsQ
pitchClassLits = mapToDataCons (mkSingLit pcFormatter "PC") ''PitchClass

-- | Generate accidental literal declarations.
accidentalLits :: DecsQ
accidentalLits = mapToDataCons (mkSingLit accFormatter "Acc") ''Accidental

-- | Generate octave literal declarations.
octaveLits :: DecsQ
octaveLits = mapToDataCons (mkSingLit octFormatter "Oct") ''OctaveNum


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
shortAccFormatter name = if accFormatter name == "fl" then "b" else [head $ accFormatter name]

-- | One letter accidental with implicit 'Naturals'.
shorterAccFormatter :: Formatter
shorterAccFormatter name = if shortAccFormatter name == "n" then "" else shortAccFormatter name

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
