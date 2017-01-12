{-# LANGUAGE TypeInType #-}

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
    , DurC
    , ChordC
    , ChordC'
    )
    where

import Mezzo.Model

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

-- | The type of duration continuations.
type DurC r d = Root r -> Music (FromRoot r d)

-- | The type of combinatorial chord type literals with default inversion.
type ChordC c r t d = Root r -> DurC r d -> Music (FromChord (c r t Inv0) d)

-- | The type of combinatorial chord type literals with custom inversion.
type ChordC' c r t i d = Root r -> Inv i -> DurC r d -> Music (FromChord (c r t i) d)
