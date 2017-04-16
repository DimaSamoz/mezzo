
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Types
-- Description :  Common types
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used by the composition library.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Types
    (
    -- * Duration type synonyms
      Whole
    , Half
    , Quarter
    , Eighth
    , Sixteenth
    , ThirtySecond
    -- * Melody
    , Melody (..)
    )
    where

import Mezzo.Model
import Mezzo.Model.Prim

import Mezzo.Compose.Builder

import Data.Kind
import GHC.TypeLits

infixl 5 :|
infixl 5 :<<<
infixl 5 :<<
infixl 5 :<
infixl 5 :^
infixl 5 :>
infixl 5 :>>
infixl 5 :<<.
infixl 5 :<.
infixl 5 :^.
infixl 5 :>.
infixl 5 :>>.
infixl 5 :~|
infixl 5 :~<<<
infixl 5 :~<<
infixl 5 :~<
infixl 5 :~^
infixl 5 :~>
infixl 5 :~>>
infixl 5 :~<<.
infixl 5 :~<.
infixl 5 :~^.
infixl 5 :~>.
infixl 5 :~>>.

-------------------------------------------------------------------------------
-- Duration type synonyms
-------------------------------------------------------------------------------

-- | Whole note duration.
type Whole = 32

-- | Half note duration.
type Half = 16

-- | Quarter note duration.
type Quarter = 8

-- | Eighth note duration.
type Eighth = 4

-- | Sixteenth note duration.
type Sixteenth = 2

-- | Thirty-second note duration.
type ThirtySecond = 1

-------------------------------------------------------------------------------
-- Musical lists
-------------------------------------------------------------------------------

-- | Single-voice melody: gives an easy way to input notes and rests of different lengths.
data Melody :: forall l t k r. Signature t k r -> Partiture 1 l -> Nat -> Type where
    -- | Start a new melody. If the first note duration is not specified ('(:|)' is used),
    -- the default duration is a quarter.
    Melody  :: Melody s (End :-- None) Quarter
    -- | Add a note with the same duration as the previous one.
    (:|)    :: (ValidMel s ms (FromRoot r d), IntRep r, Primitive d, ValidNote s r d)
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r d) d
    -- | Add a thirty-second note.
    (:<<<)  :: (ValidMel s ms (FromRoot r ThirtySecond), IntRep r, Primitive d, ValidNote s r ThirtySecond)
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r ThirtySecond) ThirtySecond
    -- | Add a sixteenth note.
    (:<<)   :: (ValidMel s ms (FromRoot r Sixteenth), IntRep r, Primitive d, ValidNote s r Sixteenth)
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r Sixteenth) Sixteenth
    -- | Add an eighth note.
    (:<)    :: (ValidMel s ms (FromRoot r Eighth), IntRep r, Primitive d, ValidNote s r Eighth)
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r Eighth) Eighth
    -- | Add a quarter note.
    (:^)    :: (ValidMel s ms (FromRoot r Quarter), IntRep r, Primitive d, ValidNote s r Quarter)
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r Quarter) Quarter
    -- | Add a half note.
    (:>)    :: (ValidMel s ms (FromRoot r Half), IntRep r, Primitive d, ValidNote s r Half)
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r Half) Half
    -- | Add a whole note.
    (:>>)   :: (ValidMel s ms (FromRoot r Whole), IntRep r, Primitive d, ValidNote s r Whole)
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r Whole) Whole
    -- | Add a dotted sixteenth note.
    (:<<.)  :: (ValidMel s ms (FromRoot r (Dot Sixteenth)), IntRep r, Primitive d, ValidNote s r (Dot Sixteenth))
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r (Dot Sixteenth)) (Dot Sixteenth)
    -- | Add a dotted eighth note.
    (:<.)   :: (ValidMel s ms (FromRoot r (Dot Eighth)), IntRep r, Primitive d, ValidNote s r (Dot Eighth))
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r (Dot Eighth)) (Dot Eighth)
    -- | Add a dotted quarter note.
    (:^.)   :: (ValidMel s ms (FromRoot r (Dot Quarter)), IntRep r, Primitive d, ValidNote s r (Dot Quarter))
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r (Dot Quarter)) (Dot Quarter)
    -- | Add a dotted half note.
    (:>.)   :: (ValidMel s ms (FromRoot r (Dot Half)), IntRep r, Primitive d, ValidNote s r (Dot Half))
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r (Dot Half)) (Dot Half)
    -- | Add a dotted whole note.
    (:>>.)  :: (ValidMel s ms (FromRoot r (Dot Whole)), IntRep r, Primitive d, ValidNote s r (Dot Whole))
           => Melody s ms d -> RootS r -> Melody s (ms +|+ FromRoot r (Dot Whole)) (Dot Whole)
    -- | Add a rest with the same duration as the previous one.
    (:~|)   :: (ValidMel s ms (FromSilence d), Primitive d, ValidRest s d)
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence d) d
    -- | Add a thirty-second rest.
    (:~<<<) :: (ValidMel s ms (FromSilence ThirtySecond), Primitive d, ValidRest s ThirtySecond)
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence ThirtySecond) ThirtySecond
    -- | Add a sixteenth rest.
    (:~<<)  :: (ValidMel s ms (FromSilence Sixteenth), Primitive d, ValidRest s Sixteenth)
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence Sixteenth) Sixteenth
    -- | Add an eighth rest.
    (:~<)   :: (ValidMel s ms (FromSilence Eighth), Primitive d, ValidRest s Eighth)
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence Eighth) Eighth
    -- | Add a quarter rest.
    (:~^)   :: (ValidMel s ms (FromSilence Quarter), Primitive d, ValidRest s Quarter)
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence Quarter) Quarter
    -- | Add a half rest.
    (:~>)   :: (ValidMel s ms (FromSilence Half), Primitive d, ValidRest s Half)
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence Half) Half
    -- | Add a whole rest.
    (:~>>)  :: (ValidMel s ms (FromSilence Whole), Primitive d, ValidRest s Whole)
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence Whole) Whole
    -- | Add a dotted sixteenth rest.
    (:~<<.) :: (ValidMel s ms (FromSilence (Dot Sixteenth)), Primitive d, ValidRest s (Dot Sixteenth))
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence (Dot Sixteenth)) (Dot Sixteenth)
    -- | Add a dotted eighth rest.
    (:~<.)  :: (ValidMel s ms (FromSilence (Dot Eighth)), Primitive d, ValidRest s (Dot Eighth))
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence (Dot Eighth)) (Dot Eighth)
    -- | Add a dotted quarter rest.
    (:~^.)  :: (ValidMel s ms (FromSilence (Dot Quarter)), Primitive d, ValidRest s (Dot Quarter))
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence (Dot Quarter)) (Dot Quarter)
    -- | Add a dotted half rest.
    (:~>.)  :: (ValidMel s ms (FromSilence (Dot Half)), Primitive d, ValidRest s (Dot Half))
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence (Dot Half)) (Dot Half)
    -- | Add a dotted whole rest.
    (:~>>.) :: (ValidMel s ms (FromSilence (Dot Whole)), Primitive d, ValidRest s (Dot Whole))
           => Melody s ms d -> RestS -> Melody s (ms +|+ FromSilence (Dot Whole)) (Dot Whole)
