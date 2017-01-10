{-# LANGUAGE TypeInType, TypeApplications, TemplateHaskell, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Harmonic
-- Description :  Harmonic composition units
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Literals for chords and progressions.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Harmonic where

import Mezzo.Model
import Mezzo.Compose.Templates
import Mezzo.Compose.Basic

-- * Atomic literals

-- ** Scale degree literals
scaleDegreeLits

root :: Pit p -> Root (PitchRoot p)
root p = Root


-- maj :: Pit p -> DurT p d -> Music (FromChord (Triad (PitchRoot p) MajTriad Inv0) d)
-- maj p d = undefined
