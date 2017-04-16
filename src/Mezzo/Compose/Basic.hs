{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Basic
-- Description :  Basic composition units
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Literals for pitches, notes, durations, etc.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Basic where

import GHC.TypeLits
import Data.Kind
import Control.Monad

import Mezzo.Model
import Mezzo.Compose.Types
import Mezzo.Compose.Builder
import Mezzo.Compose.Templates

-- * Atomic literals

-- ** Pitch class literals
pitchClassLits

-- ** Accidental literals
accidentalLits

-- ** Octave literals
octaveLits

-- ** Duration literals and terminators
mk32ndLits
join <$> traverse mkDurLits [''Whole, ''Half, ''Quarter, ''Eighth, ''Sixteenth]


-- * Pitches

-- ** Constructor

-- | Create a new pitch with the given class, accidental and octave.
pitch :: Primitive (Pitch pc acc oct) => PC pc -> Acc acc -> Oct oct -> Pit (Pitch pc acc oct)
pitch pc acc oct = Pit

-- | Value representing silence, the "pitch" of rests.
silence :: Pit Silence
silence = Pit

-- ** Concrete literals
mkPitchLits

-- ** Pitch specifiers (admitting continuations)
mkPitchSpecs

r :: RestS
r dur = dur Pit
-- | Raise a pitch by a semitone.
sharp :: RootM r (Sharpen r)
sharp = constConv Root

-- | Lower a pitch by a semitone.
flat :: RootM r (Flatten r)
flat = constConv Root

-- * Notes

-- ** Constructors
-- | Create a new root from a pitch.
rootP :: IntRep p => Pit p -> Root (PitchRoot p)
rootP p = Root

-- | Create a new root from a key and a scale degree.
rootS :: Primitive (DegreeRoot k d) => KeyS k -> Deg d -> Root (DegreeRoot k d)
rootS k d = Root

-- | Create a new note from a root and duration.
noteP :: (Primitive d, IntRep p) => Pit p -> Dur d -> Music s (FromRoot (PitchRoot p) d)
noteP p = Note (rootP p)

noteS :: (Primitive d, IntRep (DegreeRoot k sd))
      => KeyS k -> Deg sd -> Dur d -> Music s (FromRoot (DegreeRoot k sd) d)
noteS k sd = Note (rootS k sd)

-- | Create a rest from a duration.
rest :: Primitive d => Dur d -> Music s (FromSilence d)
rest = Rest
