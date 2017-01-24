{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell, RankNTypes #-}

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

-- ** Duration literals
_wh :: Whole
_wh = Dur @32

_ha :: Half
_ha = Dur @16

_qu :: Quarter
_qu = Dur @8

_ei :: Eighth
_ei = Dur @4

_si :: Dur 2
_si = Dur @2

_th :: Dur 1
_th = Dur @1

-- * Pitches

-- ** Constructor

-- | Create a new pitch with the given class, accidental and octave.
pitch :: PC pc -> Acc acc -> Oct oct -> Pit (Pitch pc acc oct)
pitch pc acc oct = Pit

-- | Value representing silence, the "pitch" of rests.
silence :: Pit Silence
silence = Pit

-- ** Concrete literals
mkPitchLits

-- ** Pitch specifiers (admitting continuations)
mkPitchSpecs

r :: RootS (PitchRoot Silence)
r = \dur -> dur (rootP silence)

-- | Raise a pitch by a semitone.
sharp :: RootM r (Sharpen r)
sharp = constConv Root

-- | Lower a pitch by a semitone.
flat :: RootM r (Flatten r)
flat = constConv Root

-- * Notes

-- ** Constructors
-- | Create a new root from a pitch.
rootP :: Pit p -> Root (PitchRoot p)
rootP p = Root

-- | Create a new root from a key and a scale degree.
rootS :: KeyS k -> ScaDeg d -> Root (DegreeRoot k d)
rootS k d = Root

-- | Create a new note from a root and duration.
noteP :: Pit p -> Dur d -> Music (FromRoot (PitchRoot p) d)
noteP p d = Note (rootP p) d

noteS :: KeyS k -> ScaDeg sd -> Dur d -> Music (FromRoot (DegreeRoot k sd) d)
noteS k sd d = Note (rootS k sd) d

-- | Create a rest from a duration.
rest :: Dur d -> Music (FromSilence d)
rest d = Rest d

-- ** Note terminators (which express the note duration)

wn :: NoteT r 32
wn = \p -> Note p _wh

hn :: NoteT r 16
hn = \p -> Note p _ha

qn :: NoteT r 8
qn = \p -> Note p _qu

en :: NoteT r 4
en = \p -> Note p _ei

sn :: NoteT r 2
sn = \p -> Note p _si

tn :: NoteT r 1
tn = \p -> Note p _th

-- ** Chord terminators (which express the note duration)

wc :: ChorT r 32
wc = \p -> Chord p _wh

hc :: ChorT r 16
hc = \p -> Chord p _ha

qc :: ChorT r 8
qc = \p -> Chord p _qu

ec :: ChorT r 4
ec = \p -> Chord p _ei

sc :: ChorT r 2
sc = \p -> Chord p _si

tc :: ChorT r 1
tc = \p -> Chord p _th
