{-# LANGUAGE TypeInType, GADTs, TypeFamilies, TypeOperators, TypeApplications, UndecidableInstances #-}

module Lib
     where

import GHC.TypeLits
import GHC.Exts
import Data.Kind hiding (Constraint)
import Data.Proxy

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data PitchClass = C | D | E | F | G | A | B deriving (Eq, Show, Enum, Ord)

data Accidental = Natural | Flat | Sharp deriving (Eq, Show, Enum, Ord)

data Oct = Oct_1 | Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6 | Oct7 | Oct8 deriving (Eq, Show, Ord, Enum, Bounded)

-- data Duration = Duration :^: Duration | Whole | Half Duration deriving (Eq, Show)

type Whole = 32
type Half = 16
type Quarter = 8
type Eighth = 4
type Sixteenth = 2
type ThirtySecond = 1


type Duration = Nat
-- Proxy
data Dur (a :: Duration) = Dur

-- type family HalfOf (d :: Duration) :: Duration where
--     HalfOf Sixteenth = ThirtySecond
--     HalfOf Eighth = Sixteenth
--     HalfOf Quarter = Eighth
--     HalfOf Half = Quarter
--     HalfOf Whole = Half
--
-- type family Dotted (u :: Unit d) :: Unit e where
--     -- Dotted ThirtySecond = TypeError (Text "Can't create a dotted thirty-second note.")
--     Dotted (Note p dur) = Note p (Dur :: Dur (dur + HalfOf(dur)))
--     Dotted Eighth = Eighth + Sixteenth
--     Dotted Quarter = Quarter + Eighth
--     Dotted Half = Half + Quarter



data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave deriving (Eq, Show, Ord)

data IntervalClass = Maj | Perf | Min | Aug | Dim deriving (Eq, Show)

data Interval = Interval IntervalClass IntervalSize deriving (Eq, Show)

-- type family Shrink (i :: Interval) :: Interval where
--     Shrink ('Interval Perf Unison) = 'Interval Perf Unison
--     Shrink ('Interval Perf )

data Pitch where
    Pitch :: PitchClass -> Accidental -> Oct -> Pitch

type family MakeInterval (p1 :: Pitch) (p2 :: Pitch) :: Interval where
    MakeInterval ('Pitch C Natural o1) ('Pitch C Natural o1) = 'Interval Perf Unison
    MakeInterval ('Pitch C Natural o1) ('Pitch D Natural o1) = 'Interval Maj Second
    MakeInterval ('Pitch C Natural o1) ('Pitch E Natural o1) = 'Interval Maj Third
    MakeInterval ('Pitch C Natural o1) ('Pitch F Natural o1) = 'Interval Perf Fourth
    MakeInterval ('Pitch C Natural o1) ('Pitch G Natural o1) = 'Interval Perf Fifth
    MakeInterval ('Pitch C Natural o1) ('Pitch A Natural o1) = 'Interval Maj Sixth
    MakeInterval ('Pitch C Natural o1) ('Pitch B Natural o1) = 'Interval Maj Seventh
    MakeInterval ('Pitch C Natural o1) ('Pitch C Natural o2) = 'Interval Perf Octave
    MakeInterval ('Pitch C Natural o1) ('Pitch pc2 Sharp o2) = 'Interval Perf Octave

x :: Proxy (MakeInterval ('Pitch C Natural Oct3) ('Pitch C Natural Oct4))
x = undefined


data Unit :: Duration -> Type where
    Note :: Pitch -> Dur d -> Unit d
    Rest :: Dur d -> Unit d

-- type family ValidDuration (d :: Duration) :: Bool where
--     ValidDuration Whole = True
--     ValidDuration (Half (Half (Half (Half (Half Whole))))) = TypeError (Text "Note duration must be longer than 1/32.")
--     ValidDuration (Half d) = ValidDuration d
--
-- -- type family ToInterval (pc1 :: PitchClass) (pc2 :: PitchClass) :: Interval where
-- --     ToInterval
--
-- type family Ensure (b :: Bool) :: Constraint where
--     Ensure v = v ~ True
--

data Music :: Nat -> Type where
    Prim :: Unit d -> Music d
    (:-:) :: Music d1 -> Music d2 -> Music (d1 + d2)
    (:|:) :: d1 ~ d2 => Music d1 -> Music d2 -> Music d1


m :: Music Half
m = Prim (Rest (Dur @Half)) :|: Prim (Rest (Dur @Half))
