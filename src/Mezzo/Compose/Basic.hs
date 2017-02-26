{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell, RankNTypes, FlexibleContexts, GADTs #-}

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
wh :: Whole
wh = Dur @32

ha :: Half
ha = Dur @16

qu :: Quarter
qu = Dur @8

ei :: Eighth
ei = Dur @4

si :: Sixteenth
si = Dur @2

th :: ThirtySecond
th = Dur @1

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
r = \dur -> dur Pit

-- | Raise a pitch by a semitone.
sharp :: RootM r (Sharpen r)
sharp = constConv Root

-- | Lower a pitch by a semitone.
flat :: RootM r (Flatten r)
flat = constConv Root

-- * Notes

-- ** Constructors
-- | Create a new root from a pitch.
rootP :: (Primitive p, Rep p ~ Int) => Pit p -> Root (PitchRoot p)
rootP p = Root

-- | Create a new root from a key and a scale degree.
rootS :: Primitive (DegreeRoot k d) => KeyS k -> ScaDeg d -> Root (DegreeRoot k d)
rootS k d = Root

-- | Create a new note from a root and duration.
noteP :: (Primitive d, Primitive p, Rep p ~ Int) => Pit p -> Dur d -> Music (FromRoot (PitchRoot p) d)
noteP p d = Note (rootP p) d

noteS :: (Primitive d, Primitive (DegreeRoot k sd), Rep (DegreeRoot k sd) ~ Int)
      => KeyS k -> ScaDeg sd -> Dur d -> Music (FromRoot (DegreeRoot k sd) d)
noteS k sd d = Note (rootS k sd) d

-- | Create a rest from a duration.
rest :: Primitive d => Dur d -> Music (FromSilence d)
rest d = Rest d

-- ** Note terminators (which express the note duration)

wn :: Rep r ~ Int => RootT r 32
wn = \p -> Note p wh

hn :: Rep r ~ Int => RootT r 16
hn = \p -> Note p ha

qn :: Rep r ~ Int => RootT r 8
qn = \p -> Note p qu

en :: Rep r ~ Int => RootT r 4
en = \p -> Note p ei

sn :: Rep r ~ Int => RootT r 2
sn = \p -> Note p si

tn :: Rep r ~ Int => RootT r 1
tn = \p -> Note p th

-- ** Rest terminators (which express the note duration)

wr :: RestT 32
wr = \p -> Rest wh

hr :: RestT 16
hr = \p -> Rest ha

qr :: RestT 8
qr = \p -> Rest qu

er :: RestT 4
er = \p -> Rest ei

sr :: RestT 2
sr = \p -> Rest si

tr :: RestT 1
tr = \p -> Rest th

-- ** Chord terminators (which express the chord duration)

wc :: Primitive n => ChorT (r :: ChordType n) 32
wc = \p -> Chord p wh

hc :: Primitive n => ChorT (r :: ChordType n) 16
hc = \p -> Chord p ha

qc :: Primitive n => ChorT (r :: ChordType n) 8
qc = \p -> Chord p qu

ec :: Primitive n => ChorT (r :: ChordType n) 4
ec = \p -> Chord p ei

sc :: Primitive n => ChorT (r :: ChordType n) 2
sc = \p -> Chord p si

tc :: Primitive n => ChorT (r :: ChordType n) 1
tc = \p -> Chord p th
