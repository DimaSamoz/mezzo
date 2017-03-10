{-# LANGUAGE TypeInType, TypeOperators, GADTs, TypeFamilies, MultiParamTypeClasses,
    FlexibleInstances, UndecidableInstances, FunctionalDependencies, FlexibleContexts, RankNTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

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
-- infixr 5 :~

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

-- | List of pitches with a common duration.
data Melody :: forall l. Partiture 1 l -> Nat -> Type where
    WithDur :: Dur d -> Melody (End :-- None) d
    (:+) :: (Primitive r, MelConstraints (FromRoot r d) ms, Rep r ~ Int) =>
                RootS r -> Melody ms d -> Melody (FromRoot r d +|+ ms) d
    (:~) :: (MelConstraints (FromSilence d) ms) =>
                RestS -> Melody ms d -> Melody (FromSilence d +|+ ms) d
    Melody :: Melody (End :-- None) Quarter
    (:|)   :: (MelConstraints ms (FromRoot r d), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r d) d
    (:<<<) :: (MelConstraints ms (FromRoot r ThirtySecond), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r ThirtySecond) ThirtySecond
    (:<<)  :: (MelConstraints ms (FromRoot r Sixteenth), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r Sixteenth) Sixteenth
    (:<)   :: (MelConstraints ms (FromRoot r Eighth), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r Eighth) Eighth
    (:^)   :: (MelConstraints ms (FromRoot r Quarter), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r Quarter) Quarter
    (:>)   :: (MelConstraints ms (FromRoot r Half), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r Half) Half
    (:>>)  :: (MelConstraints ms (FromRoot r Whole), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r Whole) Whole
    (:<<.) :: (MelConstraints ms (FromRoot r (Dot Sixteenth)), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r (Dot Sixteenth)) (Dot Sixteenth)
    (:<.)  :: (MelConstraints ms (FromRoot r (Dot Eighth)), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r (Dot Eighth)) (Dot Eighth)
    (:^.)  :: (MelConstraints ms (FromRoot r (Dot Quarter)), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r (Dot Quarter)) (Dot Quarter)
    (:>.)  :: (MelConstraints ms (FromRoot r (Dot Half)), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r (Dot Half)) (Dot Half)
    (:>>.) :: (MelConstraints ms (FromRoot r (Dot Whole)), Primitive r, Primitive d, Rep r ~ Int)
           => Melody ms d -> RootS r -> Melody (ms +|+ FromRoot r (Dot Whole)) (Dot Whole)

