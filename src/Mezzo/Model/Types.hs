{-# LANGUAGE TypeInType, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Types
-- Description :  Mezzo music types
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types modeling basic musical constructs at the type level.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Types
    (
    -- * Note properties
      PitchClass (..)
    , Accidental (..)
    , OctaveNum (..)
    , Duration (..)
    -- ** Singleton types for note properties
    , PC (..)
    , Acc (..)
    , Oct (..)
    , Dur (..)
    -- * Pitches
    , PitchType (..)
    -- * Intervals
    , IntervalSize (..)
    , IntervalClass (..)
    , IntervalType (..)
    ) where

import GHC.TypeLits

import Mezzo.Model.Prim

-------------------------------------------------------------------------------
-- Note properties
-- The "minimum complete definition" for musical notes and rests.
-------------------------------------------------------------------------------

-- | The diatonic pitch class of the note.
data PitchClass = C | D | E | F | G | A | B

-- | The accidental applied to a note.
data Accidental = Natural | Flat | Sharp

-- | The octave where the note resides (middle C is Oct3).
data OctaveNum =
    Oct_1 | Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6 | Oct7 | Oct8

-- | The duration of the note (a whole note has duration 32).
type Duration = Nat

---- Singleton types for note properties

-- | The singleton type for 'PitchClass'.
data PC (pc :: PitchClass) where
    PC :: PC pc

-- | The singleton type for 'Accidental'.
data Acc (acc :: Accidental) where
    Acc :: Acc acc

-- | The singleton type for 'Octave'.
data Oct (oct :: OctaveNum) where
    Oct :: Oct oct

-- | The singleton type for 'Duration'.
data Dur (dur :: Duration) where
    Dur :: Dur dur

-------------------------------------------------------------------------------
-- Pitches
-- Encapsulates the pitch class, accidental and octave of a note.
-------------------------------------------------------------------------------

-- | The type of pitches.
data PitchType where
    -- | A pitch made up of a pitch class, an accidental and an octave.
    Pitch :: PitchClass -> Accidental -> OctaveNum -> PitchType
    -- | Silence, the pitch of rests.
    Silence :: PitchType

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

-- | The size of the interval.
data IntervalSize =
    Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave

-- | The class of the interval.
data IntervalClass = Maj | Perf | Min | Aug | Dim

-- | The type of intervals.
data IntervalType where
    -- | An interval smaller than 13 semitones, where musical rules
    -- can still be enforced.
    Interval :: IntervalClass -> IntervalSize -> IntervalType
    -- | An interval larger than 13 semitones, which is large enough
    -- so that dissonance effects are not significant.
    Compound :: IntervalType
