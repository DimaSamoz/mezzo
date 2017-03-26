
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Intervals
-- Description :  Interval literals
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Literals and operations involving intervals.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Intervals where

import Mezzo.Model
import Mezzo.Model.Prim
import Mezzo.Compose.Types
import Mezzo.Compose.Builder
import Mezzo.Compose.Templates

import GHC.TypeLits
import Data.Kind
import Control.Monad


-- * Atomic literals

-- ** Interval class literals

_iMaj :: IC Maj
_iMaj = IC

_iMin :: IC Min
_iMin = IC

_iPerf :: IC Perf
_iPerf = IC

_iAug :: IC Aug
_iAug = IC

_iDim :: IC Dim
_iDim = IC

-- ** Interval size literals

_i1 :: IS Unison
_i1 = IS

_i2 :: IS Second
_i2 = IS

_i3 :: IS Third
_i3 = IS

_i4 :: IS Fourth
_i4 = IS

_i5 :: IS Fifth
_i5 = IS

_i6 :: IS Sixth
_i6 = IS

_i7 :: IS Seventh
_i7 = IS

_i8 :: IS Octave
_i8 = IS

-- ** Constructor

interval :: IC ic -> IS is -> Intv (Interval ic is)
interval _ _ = Intv

-- * Concrete interval literals

iPerf1 :: Intv (Interval Perf Unison)
iPerf1 = Intv

iAug1 :: Intv (Interval Aug Unison)
iAug1 = Intv


iDim2 :: Intv (Interval Dim Second)
iDim2 = Intv

iMin2 :: Intv (Interval Min Second)
iMin2 = Intv

iMaj2 :: Intv (Interval Maj Second)
iMaj2 = Intv

iAug2 :: Intv (Interval Aug Second)
iAug2 = Intv


iDim3 :: Intv (Interval Dim Third)
iDim3 = Intv

iMin3 :: Intv (Interval Min Third)
iMin3 = Intv

iMaj3 :: Intv (Interval Maj Third)
iMaj3 = Intv

iAug3 :: Intv (Interval Aug Third)
iAug3 = Intv


iDim4 :: Intv (Interval Dim Fourth)
iDim4 = Intv

iPerf4 :: Intv (Interval Perf Fourth)
iPerf4 = Intv

iAug4 :: Intv (Interval Aug Fourth)
iAug4 = Intv


iDim5 :: Intv (Interval Dim Fifth)
iDim5 = Intv

iPerf5 :: Intv (Interval Perf Fifth)
iPerf5 = Intv

iAug5 :: Intv (Interval Aug Fifth)
iAug5 = Intv


iDim6 :: Intv (Interval Dim Sixth)
iDim6 = Intv

iMin6 :: Intv (Interval Min Sixth)
iMin6 = Intv

iMaj6 :: Intv (Interval Maj Sixth)
iMaj6 = Intv

iAug6 :: Intv (Interval Aug Sixth)
iAug6 = Intv


iDim7 :: Intv (Interval Dim Seventh)
iDim7 = Intv

iMin7 :: Intv (Interval Min Seventh)
iMin7 = Intv

iMaj7 :: Intv (Interval Maj Seventh)
iMaj7 = Intv

iAug7 :: Intv (Interval Aug Seventh)
iAug7 = Intv


iDim8 :: Intv (Interval Dim Octave)
iDim8 = Intv

iPerf8 :: Intv (Interval Perf Octave)
iPerf8 = Intv

iAug8 :: Intv (Interval Aug Octave)
iAug8 = Intv


-- * Operations

transUp :: (tr ~ (PitchRoot (RaiseBy (RootToPitch r) i)), IntRep tr)
    => Intv i -> RootS r -> RootS tr
transUp i _ = spec Root

transDown :: (tr ~ (PitchRoot (LowerBy (RootToPitch r) i)), IntRep tr)
    => Intv i -> RootS r -> RootS tr
transDown i _ = spec Root
