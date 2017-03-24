
----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Harmony.Motion
-- Description :  Models of harmonic motion
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Classes modelling consonance, dissonance and harmonic motion.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Harmony.Motion
    (
    -- * Consonant and dissonant intervals
      PerfConsonantInterval
    , ImperfConsonantInterval
    , DissonantInterval
    -- * Harmonic motion
    , DirectMotion
    , ContraryMotion
    , ObliqueMotion
    ) where

import GHC.TypeLits

import Mezzo.Model.Types
import Mezzo.Model.Prim
import Mezzo.Model.Errors

-------------------------------------------------------------------------------
-- Consonant and dissonant intervals
-------------------------------------------------------------------------------

-- | Classifies perfect consonant intervals:
--
--  * Perfect unisons
--  * Perfect fifths
--  * Perfect octaves
class PerfConsonantInterval (i :: IntervalType)
instance PerfConsonantInterval (Interval Perf Unison)
instance PerfConsonantInterval (Interval Perf Fifth)
instance PerfConsonantInterval (Interval Perf Octave)

-- | Classifies imperfect consonant intervals:
--
--  * Major and minor thirds
--  * Major and minor sixths
class ImperfConsonantInterval (i :: IntervalType)
instance ImperfConsonantInterval (Interval Maj Third)
instance ImperfConsonantInterval (Interval Min Third)
instance ImperfConsonantInterval (Interval Maj Sixth)
instance ImperfConsonantInterval (Interval Min Sixth)

-- | Classifies dissonant intervals:
--
--  * Perfect fourth (by common practice convention)
--  * Augmented and diminished intervals
--  * Second and seventh intervals
class DissonantInterval (i :: IntervalType)
instance DissonantInterval (Interval Perf Fourth)
instance DissonantInterval (Interval Aug is)
instance DissonantInterval (Interval Dim is)
instance DissonantInterval (Interval ic Second)
instance DissonantInterval (Interval ic Seventh)

-------------------------------------------------------------------------------
-- Harmonic motion
-------------------------------------------------------------------------------

-- | Ensures that direct motion is permitted between the two intervals.
class DirectMotion (e :: DyadPair) (i1 :: IntervalType) (i2 :: IntervalType)
instance {-# OVERLAPPING #-} MotionError "Parallel unisons are forbidden: " e
                                => DirectMotion e (Interval Perf Unison) (Interval Perf Unison)
instance {-# OVERLAPS #-} MotionError "Direct motion into a perfect unison is forbidden: " e
                                => DirectMotion e i1 (Interval Perf Unison)
instance {-# OVERLAPPING #-} MotionError "Parallel fifths are forbidden: " e
                                => DirectMotion e (Interval Perf Fifth) (Interval Perf Fifth)
instance {-# OVERLAPS #-} MotionError "Direct motion into a perfect fifth is forbidden: " e
                                => DirectMotion e i1 (Interval Perf Fifth)
instance {-# OVERLAPPING #-} MotionError "Parallel octaves are forbidden: " e
                                => DirectMotion e (Interval Perf Octave) (Interval Perf Octave)
instance {-# OVERLAPS #-} MotionError "Direct motion into a perfect octave is forbidden: " e
                                => DirectMotion e i1 (Interval Perf Octave)
instance {-# OVERLAPPABLE #-}      DirectMotion e i1 i2

-- | Ensures that contrary motion is permitted between the two intervals.
class ContraryMotion (i1 :: IntervalType) (i2 :: IntervalType)
instance ContraryMotion i1 i2

-- | Ensures that oblique motion is permitted between the two intervals.
class ObliqueMotion (i1 :: IntervalType) (i2 :: IntervalType)
instance ObliqueMotion i1 i2
