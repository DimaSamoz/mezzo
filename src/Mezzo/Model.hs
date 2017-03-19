
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model
-- Description :  Mezzo music model
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Module providing the external interface to the Mezzo type-level music model.
--
-----------------------------------------------------------------------------

module Mezzo.Model (module X) where

-- Uses import/export shortcut as suggested by HLint.

import Mezzo.Model.Types as X
    (
      PitchClass (..)
    , Accidental (..)
    , OctaveNum (..)
    , Duration (..)
    , PC (..)
    , Acc (..)
    , Oct (..)
    , Dur (..)
    , PitchType (..)
    , Pit (..)
    , Mode (..)
    , ScaleDegree (..)
    , DegreeType (..)
    , KeyType (..)
    , RootType (..)
    , Mod (..)
    , ScaDeg (..)
    , Deg (..)
    , KeyS (..)
    , Root (..)
    , RootToPitch
    , PitchToNat
    , Sharpen
    , Flatten
    , Dot
    , FromRoot
    , FromSilence
    , Voice
    , Partiture
    , RaiseAllByOct
    )
import Mezzo.Model.Harmony.Chords as X
import Mezzo.Model.Harmony.Functional as X
import Mezzo.Model.Music as X
import Mezzo.Model.Reify as X
