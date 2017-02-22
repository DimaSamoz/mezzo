{-# LANGUAGE TypeInType, TypeApplications, TypeOperators, FlexibleContexts, RankNTypes,
    FlexibleInstances, ScopedTypeVariables, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Combine
-- Description :  Music combinators
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Properties and combinators for 'Music' values.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Combine
    (
    -- * Music properties and padding
      musicDur
    , durToInt
    , duration
    , voices
    , pad
    -- * Melody composition
    , melody
    , withDur
    ) where

import Mezzo.Model
import Mezzo.Compose.Basic
import Mezzo.Compose.Builder
import Mezzo.Compose.Types
import Mezzo.Model.Prim
import Mezzo.Model.Types
import Mezzo.Model.Music

import GHC.TypeLits

-------------------------------------------------------------------------------
-- Music properties
-------------------------------------------------------------------------------

-- | Get the duration of a piece of music.
musicDur :: Primitive l => Music (m :: Partiture n l) -> Dur l
musicDur _ = Dur

-- | Convert a duration to an integer.
durToInt :: Primitive d => Dur d -> Int
durToInt = prim

-- | Get the numeric duration of a piece of music.
duration :: Primitive l => Music (m :: Partiture n l) -> Int
duration = durToInt . musicDur

-- | Get the number of voices in a piece of music.
voices :: Music m -> Int
voices (Note r d) = 1
voices (Rest d) = 1
voices (m1 :|: m2) = voices m1
voices (m1 :-: m2) = voices m1 + voices m2
voices (Chord c d) = chordVoices c

-- | Get the number of voices in a chord.
-- Thanks to Michael B. Gale
chordVoices :: forall (n :: Nat) (c :: ChordType n) . Primitive n => Cho c -> Int
chordVoices _ = prim (undefined :: ChordType n) -- Need to get a kind-level variable to the term level

-- | Add an empty voice to the end of a piece of music.
pad :: (HarmConstraints m (FromSilence b), KnownNat b) => Music (m :: Partiture a b) -> Music (m +-+ FromSilence b)
pad m = m :-: rest (musicDur m)

-------------------------------------------------------------------------------
-- Melodies
-------------------------------------------------------------------------------

-- | Create a melody (a sequence of pitches of the same duration).
melody :: Primitive n => Melody m n -> Music m
melody (WithDur d) = rest (Dur :: Dur 0)
melody (p :+ ps) = p (`Note` notesDur) :|: melody ps
    where notesDur = getMelodyDur ps

-- | Specify the duration of a melody.
withDur :: NoteT r d -> Melody (End :-- None) d
withDur = WithDur

-- | Get the duration of the notes in a melody.
getMelodyDur :: Primitive d => Melody m d -> Dur d
getMelodyDur _ = Dur
