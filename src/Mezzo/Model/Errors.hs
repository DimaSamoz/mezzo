
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Errors
-- Description :  Musical error handling
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and functions for handling and displaying composition errors.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Errors where

import Mezzo.Model.Types

import GHC.TypeLits

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A pair of pitches.
type PitchPair = (PitchType, PitchType)

-- | A pair of dyads (pair of pairs of pitches).
data DyadPair = DyP PitchPair PitchPair

-- | Create dyad pair from four pitches.
type family DyPair p1 p2 q1 q2 :: DyadPair where
    DyPair p1 p2 q1 q2 = DyP '(p1, p2) '(q1, q2)

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

-- | Print pitch class type.
type family PpPC (pc :: PitchClass) :: ErrorMessage where
    PpPC C = Text "C"
    PpPC D = Text "D"
    PpPC E = Text "E"
    PpPC F = Text "F"
    PpPC G = Text "G"
    PpPC A = Text "A"
    PpPC B = Text "B"

-- | Print accidental type.
type family PpAcc (acc :: Accidental) :: ErrorMessage where
    PpAcc Natural = Text ""
    PpAcc Sharp = Text "#"
    PpAcc Flat = Text "b"

-- | Print octave type.
type family PpOct (oct :: OctaveNum) :: ErrorMessage where
    PpOct Oct_1 = Text "_5"
    PpOct Oct0 = Text "_4"
    PpOct Oct1 = Text "_3"
    PpOct Oct2 = Text "__"
    PpOct Oct3 = Text "_"
    PpOct Oct4 = Text ""
    PpOct Oct5 = Text "'"
    PpOct Oct6 = Text "''"
    PpOct Oct7 = Text "'3"
    PpOct Oct8 = Text "'4"

-- | Print pitch type.
type family PpPitch (p :: PitchType) :: ErrorMessage where
    PpPitch (Pitch pc acc oct) = PpPC pc :<>: PpAcc acc :<>: PpOct oct
    PpPitch Silence = Text "Rest"

-- | Print pitch pair.
type family PpPitchPair (pp :: PitchPair) :: ErrorMessage where
    PpPitchPair '(p1, p2) = PpPitch p1 :<>: Text " and " :<>: PpPitch p2

-- | Print dyad pair.
type family PpDyadPair (dp :: DyadPair) :: ErrorMessage where
    PpDyadPair (DyP d1 d2) = PpPitchPair d1 :<>: Text ", then " :<>: PpPitchPair d2

-- | Create an error message with a given text and pitch.
type family PitchError (t :: Symbol) (p :: PitchType) :: ErrorMessage where
    PitchError t p = TypeError (Text t :<>: PpPitch p)

-- | Create an error message with a given text and pair of pitches.
type family PitchPairError (t :: Symbol) (p :: PitchPair) where
    PitchPairError t p = TypeError (Text t :<>: PpPitchPair p)

-- | Create an error message with the given text and pair of dyads.
type family MotionError (t :: Symbol) (d :: DyadPair) where
    MotionError t p = TypeError (Text t :<>: PpDyadPair p)

-- | Create an error message with the given text and chord root.
type family ChordError (t1 :: Symbol) (r :: RootType) (t2 :: Symbol) where
    ChordError t1 r t2 = TypeError (Text t1  :<>: PpPitch (RootToPitch r) :<>: Text t2)
