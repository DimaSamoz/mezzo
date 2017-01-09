{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell #-}

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
-- Literals for pitches, notes, durations, chords, etc.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Basic where

import Mezzo.Model
import Mezzo.Compose.Templates

-- * Atomic literals

-- ** Pitch class literals
pitchClassLits

-- ** Accidental literals
accidentalLits

-- ** Octave literals
octaveLits

-- ** Duration literals
wh :: Dur 32
wh = Dur @32

ha :: Dur 16
ha = Dur @16

qu :: Dur 8
qu = Dur @8

ei :: Dur 4
ei = Dur @4

si :: Dur 2
si = Dur @2

th :: Dur 1
th = Dur @1

-- ** Pitch literals

-- *** Constructor

-- | Create a new pitch with the given class, accidental and octave.
pitch :: PC pc -> Acc acc -> Oct oct -> Pit (Pitch pc acc oct)
pitch pc acc oct = Pit

-- *** Literals
mkPitchLits

