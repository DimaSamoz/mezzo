
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
    , restWhile
    , pad
    , pad2
    , pad3
    , pad4
    -- * Melody composition
    , play
    , melody
    -- * Textures
    , hom
    , melAccomp
    ) where

import Mezzo.Model
import Mezzo.Compose.Basic
import Mezzo.Compose.Builder
import Mezzo.Compose.Types
import Mezzo.Compose.Harmony
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
voices (Chord c d) = chordVoices c
voices (Progression _ _) = 4
voices (Homophony m a) = voices m + voices a
voices (m1 :|: m2) = voices m1
voices (m1 :-: m2) = voices m1 + voices m2

-- | Get the number of voices in a chord.
-- Thanks to Michael B. Gale
chordVoices :: forall (n :: Nat) (c :: ChordType n) . Primitive n => Cho c -> Int
chordVoices _ = prim (undefined :: ChordType n) -- Need to get a kind-level variable to the term level

-- | Add an empty voice to the piece of music.
pad :: (ValidHarm m (FromSilence b), Primitive b)
    => Music (m :: Partiture (a - 1) b) -> Music ((m +-+ FromSilence b) :: Partiture a b)
pad m = m :-: restWhile m

-- | Add two empty voices to the piece of music.
pad2 :: ( ValidHarm m (FromSilence b)
        , ValidHarm (m +-+ FromSilence b) (FromSilence b)
        , Primitive b)
     => Music (m :: Partiture (a - 2) b) -> Music ((m +-+ FromSilence b +-+ FromSilence b) :: Partiture a b)
pad2 m = m :-: restWhile m :-: restWhile m

-- | Add three empty voices to the piece of music.
pad3 :: ( ValidHarm m (FromSilence b)
        , ValidHarm (m +-+ FromSilence b) (FromSilence b)
        , ValidHarm (m +-+ FromSilence b +-+ FromSilence b) (FromSilence b)
        , Primitive b)
     => Music (m :: Partiture (a - 3) b) -> Music ((m +-+ FromSilence b +-+ FromSilence b +-+ FromSilence b) :: Partiture a b)
pad3 m = m :-: restWhile m :-: restWhile m :-: restWhile m

-- | Add four empty voices to the piece of music.
pad4 :: ( ValidHarm m (FromSilence b)
        , ValidHarm (m +-+ FromSilence b) (FromSilence b)
        , ValidHarm (m +-+ FromSilence b +-+ FromSilence b) (FromSilence b)
        , ValidHarm (m +-+ FromSilence b +-+ FromSilence b +-+ FromSilence b) (FromSilence b)
        , Primitive b)
     => Music (m :: Partiture (a - 4) b) -> Music ((m +-+ FromSilence b +-+ FromSilence b +-+ FromSilence b +-+ FromSilence b) :: Partiture a b)
pad4 m = m :-: restWhile m :-: restWhile m :-: restWhile m :-: restWhile m

-- | Rest for the duration of the given music piece.
restWhile :: Primitive l =>  Music (m :: Partiture n l) -> Music (FromSilence l)
restWhile m = rest (musicDur m)

-------------------------------------------------------------------------------
-- Melodies
-------------------------------------------------------------------------------

-- | Convert a melody (a sequence of notes and rests) to `Music`.
play :: (Primitive d) => Melody m d -> Music m
play m@(ps :| p)    = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :<<< p)  = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :<< p)   = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :< p)    = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :^ p)    = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :> p)    = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :>> p)   = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :<<. p)  = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :<. p)   = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :^. p)   = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :>. p)   = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :>>. p)  = case ps of Melody -> mkMelNote m p ; ps' -> play ps' :|: mkMelNote m p
play m@(ps :~| p)   = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~<<< p) = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~<< p)  = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~< p)   = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~^ p)   = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~> p)   = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~>> p)  = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~<<. p) = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~<. p)  = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~^. p)  = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~>. p)  = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m
play m@(ps :~>>. p) = case ps of Melody -> mkMelRest m ; ps' -> play ps'   :|: mkMelRest m

-- | Make a note of suitable duration from a root specifier.
mkMelNote :: (IntRep r, Primitive d) => Melody m d -> RootS r -> Music (FromRoot r d)
mkMelNote m p = p (\r -> Note r (melDur m))

-- | Make a rest of suitable duration from a rest specifier.
mkMelRest :: Primitive d => Melody m d -> Music (FromSilence d)
mkMelRest m = r (\_ -> Rest (melDur m))

-- | Alias for the start of the melody.
melody :: Melody (End :-- None) Quarter
melody = Melody

-- | Get the duration of the notes in a melody.
melDur :: Primitive d => Melody m d -> Dur d
melDur _ = Dur

-------------------------------------------------------------------------------
-- Textures
-------------------------------------------------------------------------------

hom :: ValidHom m a => Music m -> Music a -> Music (m +-+ a)
hom = Homophony

melAccomp :: (ValidProg t p, pm ~ FromProg p t, ValidHom m pm, Primitive d) => KeyS k -> TimeSig t -> Melody m d -> InKey k (PhraseList p) -> Music (m +-+ pm)
melAccomp k t m p = Homophony (play m) (Progression t (inKey k p))
