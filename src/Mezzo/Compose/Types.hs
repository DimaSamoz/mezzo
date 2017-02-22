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

infixr 5 :+

-------------------------------------------------------------------------------
-- Duration type synonyms
-------------------------------------------------------------------------------

-- | Whole note duration.
type Whole = Dur 32

-- | Half note duration.
type Half = Dur 16

-- | Quarter note duration.
type Quarter = Dur 8

-- | Eighth note duration.
type Eighth = Dur 4

-- | Sixteenth note duration.
type Sixteenth = Dur 2

-- | Thirty-second note duration.
type ThirtySecond = Dur 1

-------------------------------------------------------------------------------
-- Musical lists
-------------------------------------------------------------------------------

-- | List of pitches with a common duration.
data Melody :: forall l. Partiture 1 l -> Nat -> Type where
    WithDur :: NoteT r d -> Melody (End :-- None) d
    (:+) :: (Primitive r, MelConstraints (FromRoot r d) ms) =>
                RootS r -> Melody ms d -> Melody (FromRoot r d +|+ ms) d
