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

type family Shrink (i :: Interval) :: Interval where
    Shrink ('Interval Perf Unison) = 'Interval Perf Unison
    Shrink ('Interval Perf is) = 'Interval Dim is
    Shrink ('Interval Min is) = 'Interval Dim is
    Shrink ('Interval Maj is) = 'Interval Min is

type family Expand (i :: Interval) :: Interval where
    Expand ('Interval Perf Octave) = 'Interval Perf Octave
    Expand ('Interval Perf is) = 'Interval Aug is
    Expand ('Interval Maj is) = 'Interval Aug is
    Expand ('Interval Min is) = 'Interval Maj is

type family OctSucc (o :: Oct) :: Oct where
    OctSucc Oct_1 = Oct0
    OctSucc Oct0 = Oct1
    OctSucc Oct1 = Oct2
    OctSucc Oct2 = Oct3
    OctSucc Oct3 = Oct4
    OctSucc Oct4 = Oct5
    OctSucc Oct5 = Oct6
    OctSucc Oct6 = Oct7
    OctSucc Oct7 = Oct8
    OctSucc Oct8 = TypeError (Text "Octave too high.")

type family OctPred (o :: Oct) :: Oct where
    OctPred Oct_1 = TypeError (Text "Octave too low.")
    OctPred Oct0 = Oct_1
    OctPred Oct1 = Oct0
    OctPred Oct2 = Oct1
    OctPred Oct3 = Oct2
    OctPred Oct4 = Oct3
    OctPred Oct5 = Oct4
    OctPred Oct6 = Oct5
    OctPred Oct7 = Oct6
    OctPred Oct8 = Oct7

type family ClassSucc (c :: PitchClass) :: PitchClass where
    ClassSucc C = D
    ClassSucc D = E
    ClassSucc E = F
    ClassSucc F = G
    ClassSucc G = A
    ClassSucc A = B
    ClassSucc B = C

type family ClassPred (c :: PitchClass) :: PitchClass where
    ClassPred C = B
    ClassPred D = C
    ClassPred E = D
    ClassPred F = E
    ClassPred G = F
    ClassPred A = G
    ClassPred B = A


data Pitch where
    Pitch :: PitchClass -> Accidental -> Oct -> Pitch

type family HalfStepUp (p :: Pitch) :: Pitch where
    HalfStepUp ('Pitch B acc o) = 'Pitch C acc (OctSucc o)
    HalfStepUp ('Pitch E acc o) = 'Pitch F acc o
    HalfStepUp ('Pitch pc Flat o) = 'Pitch pc Natural o
    HalfStepUp ('Pitch pc Natural o) = 'Pitch pc Sharp o
    HalfStepUp ('Pitch pc Sharp o) = 'Pitch (ClassSucc pc) Sharp o

type family MakeInterval (p1 :: Pitch) (p2 :: Pitch) :: Interval where
    MakeInterval ('Pitch C Natural o1) ('Pitch C Natural o1) = 'Interval Perf Unison
    MakeInterval ('Pitch C Natural o1) ('Pitch D Natural o1) = 'Interval Maj Second
    MakeInterval ('Pitch C Natural o1) ('Pitch E Natural o1) = 'Interval Maj Third
    MakeInterval ('Pitch C Natural o1) ('Pitch F Natural o1) = 'Interval Perf Fourth
    MakeInterval ('Pitch C Natural o1) ('Pitch G Natural o1) = 'Interval Perf Fifth
    MakeInterval ('Pitch C Natural o1) ('Pitch A Natural o1) = 'Interval Maj Sixth
    MakeInterval ('Pitch C Natural o1) ('Pitch B Natural o1) = 'Interval Maj Seventh
    MakeInterval ('Pitch C Natural o1) ('Pitch C Natural o2) = 'Interval Perf Octave
    MakeInterval ('Pitch C Natural o1) ('Pitch pc2 Sharp o2) =
            Expand (MakeInterval ('Pitch C Natural o1) ('Pitch pc2 Natural o2))
    MakeInterval ('Pitch C Natural o1) ('Pitch pc2 Flat o2) =
            Shrink (MakeInterval ('Pitch C Natural o1) ('Pitch pc2 Natural o2))

    MakeInterval _ _ = TypeError (Text "Invalid interval.")

x :: Proxy (MakeInterval ('Pitch C Natural (OctSucc Oct8)) ('Pitch F Sharp Oct3))
x = undefined

y :: Proxy (OctSucc Oct8)
y = undefined

type family ValidPitch (p :: Pitch) :: Bool where
    ValidPitch ('Pitch pc acc o) = TypeError (Text "Octave too high.")
    -- ValidPitch _ = True

-- z :: Unit 4
-- z = Note (Proxy @('Pitch C Natural (OctSucc Oct8))) (Dur @Eighth)

z :: Proxy (Note (Proxy ('Pitch C Natural Oct3)) (Dur Half))
z = undefined

data Unit :: Duration -> Type where
    Note :: Ensure (ValidPitch p) => Proxy p -> Dur d -> Unit d
    Rest :: Dur d -> Unit d

-- type family ValidDuration (d :: Duration) :: Bool where
--     ValidDuration Whole = True
--     ValidDuration (Half (Half (Half (Half (Half Whole))))) = TypeError (Text "Note duration must be longer than 1/32.")
--     ValidDuration (Half d) = ValidDuration d
--
-- -- type family ToInterval (pc1 :: PitchClass) (pc2 :: PitchClass) :: Interval where
-- --     ToInterval
--
type family Ensure (b :: Bool) :: Constraint where
    Ensure v = v ~ True


data Music :: Nat -> Type where
    Prim :: Unit d -> Music d
    (:-:) :: Music d1 -> Music d2 -> Music (d1 + d2)
    (:|:) :: d1 ~ d2 => Music d1 -> Music d2 -> Music d1


m :: Music Half
m = Prim (Note (Proxy @('Pitch C Natural Oct3)) (Dur @Half)) :|: Prim (Rest (Dur @Half))
