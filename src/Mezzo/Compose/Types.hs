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

-- | The type of duration continuations
type DurC p d = Pit p -> Music (FromPitch p d)
