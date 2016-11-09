{-# LANGUAGE TypeInType, GADTs, TypeFamilies, TypeOperators, TypeApplications, UndecidableInstances, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Lib
     where

import GHC.TypeLits
-- import GHC.Exts
import Data.Kind
import Data.Proxy

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data PitchClass = C | D | E | F | G | A | B deriving (Eq, Show, Enum, Ord)

data Accidental = Natural | Flat | Sharp deriving (Eq, Show, Enum, Ord)

data Oct = Oct_1 | Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6 | Oct7 | Oct8 deriving (Eq, Show, Ord, Enum, Bounded)

-- data Oct = Nat

-- data Duration = Duration :^: Duration | Whole | Half Duration deriving (Eq, Show)

type Whole = 32
type Half = 16
type Quarter = 8
type Eighth = 4
type Sixteenth = 2
type ThirtySecond = 1


type Duration = Nat
-- Proxy
data Dur (a :: Duration) where
    Dur :: Dur a

data Pitch :: PitchClass -> Accidental -> Oct -> Type where
    Pitch :: Pitch pc acc o

data Note :: PitchClass -> Accidental -> Oct -> Duration -> Type where
    Note :: Pitch pc acc o -> Dur d -> Note pc acc o d

data Rest :: Duration -> Type where
    Rest :: Dur d -> Rest d

data Unit :: Duration -> Type where
    NoteU :: Note p acc o d -> Unit d
    RestU :: Rest d -> Unit d

n :: Note C Natural Oct3 (Dotted Half)
n = Note Pitch Dur

u :: Unit (Dotted Half)
u = NoteU n

type family HalfOf (d :: Duration) :: Duration where
    HalfOf ThirtySecond = TypeError (Text "Note duration can't be smaller than 1/32")
    HalfOf Sixteenth = ThirtySecond
    HalfOf Eighth = Sixteenth
    HalfOf Quarter = Eighth
    HalfOf Half = Quarter
    HalfOf Whole = Half

type family Dotted (d :: Duration) :: Duration where
    Dotted d = d + HalfOf d

-- data Vec :: Type -> Nat -> Type where
--     Nil :: Vec a 0
--     (:-) :: a -> Vec a (n - 1) -> Vec a n
--
-- type family (x :: Vec a n) ++ (y :: Vec a m) :: Vec a (n + m) where
--     Nil ++ y = y
--     (h :- t) ++ y = h :- (t ++ y)
--
-- type Matrix a p q = Vec (Vec a q) p
--
-- type family (a :: Matrix t p q) +-+ (b :: Matrix t p r) :: Matrix t p (q + r) where
--     Nil         +-+ Nil         = Nil
--     (r1 :- rs1) +-+ (r2 :- rs2) = (r1 ++ r2) :- (rs1 +-+ rs2)
--
-- type family (a :: Matrix t p r) +|+ (b :: Matrix t q r) :: Matrix t (p + q) r where
--     m1 +|+ m2 = m1 ++ m2

-- m :: Proxy (((1 :- (2 :- Nil)) :- ((3 :- (4 :- Nil)) :- Nil)) +-+ ((6 :- Nil) :- ((6 :- Nil) :- Nil)))
-- m = undefined

-- data Music :: Matrix (Unit d) p q -> Type where
--     -- Prim :: (x :: Unit d) -> Music (Matrix x 1 1)
--     (:|:) :: Music m1 -> Music m2 -> Music (m1 +|+ m2)
--     (:-:) :: Music m1 -> Music m2 -> Music (m1 +-+ m2)



data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave deriving (Eq, Show, Ord)

data IntervalClass = Maj | Perf | Min | Aug | Dim deriving (Eq, Show)

-- data Interval = Interval IntervalClass IntervalSize deriving (Eq, Show)

data Interval :: IntervalClass -> IntervalSize -> Type where
    Interval :: Interval ic is

type family Shrink i where
    Shrink (Interval Perf Unison) = Interval Perf Unison
    Shrink (Interval Perf is) = Interval Dim is
    Shrink (Interval Min is) = Interval Dim is
    Shrink (Interval Maj is) = Interval Min is
    Shrink (Interval Aug Unison) = Interval Perf Unison
    Shrink (Interval Aug Fourth) = Interval Perf Fourth
    Shrink (Interval Aug Fifth) = Interval Perf Fifth
    Shrink (Interval Aug Octave) = Interval Perf Octave
    Shrink (Interval Aug is) = Interval Maj is
    Shrink (Interval Dim Unison) = TypeError (Text "Can't have diminished unisons.")
    Shrink (Interval Dim is) = Interval Maj (IntSizePred is)

type family Expand i where
    Expand (Interval Perf Octave) = Interval Perf Octave
    Expand (Interval Perf is) = Interval Aug is
    Expand (Interval Maj is) = Interval Aug is
    Expand (Interval Min is) = Interval Maj is

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
    OctSucc Oct8 = Oct8

type family OctPred (o :: Oct) :: Oct where
    OctPred Oct_1 = Oct_1
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


type family IntSizeSucc (is :: IntervalSize) :: IntervalSize where
    IntSizeSucc Unison = Second
    IntSizeSucc Second = Third
    IntSizeSucc Third = Fourth
    IntSizeSucc Fourth = Fifth
    IntSizeSucc Fifth = Sixth
    IntSizeSucc Sixth = Seventh
    IntSizeSucc Seventh = Octave
    IntSizeSucc Octave = Octave

type family IntSizePred (is :: IntervalSize) :: IntervalSize where
    IntSizePred Unison = Unison
    IntSizePred Second = Unison
    IntSizePred Third = Second
    IntSizePred Fourth = Third
    IntSizePred Fifth = Fourth
    IntSizePred Sixth = Fifth
    IntSizePred Seventh = Sixth
    IntSizePred Octave = Seventh

type family HalfStepUp p where
    HalfStepUp (Pitch B acc o) = Pitch C acc (OctSucc o)
    HalfStepUp (Pitch E acc o) = Pitch F acc o
    HalfStepUp (Pitch pc Flat o) = Pitch pc Natural o
    HalfStepUp (Pitch pc Natural o) = Pitch pc Sharp o
    HalfStepUp (Pitch pc Sharp o) = Pitch (ClassSucc pc) Sharp o

type family MakeInterval p1 p2 where
    MakeInterval (Pitch C Natural o1) (Pitch C Natural o1) = Interval Perf Unison
    MakeInterval (Pitch C Natural o1) (Pitch D Natural o1) = Interval Maj Second
    MakeInterval (Pitch C Natural o1) (Pitch E Natural o1) = Interval Maj Third
    MakeInterval (Pitch C Natural o1) (Pitch F Natural o1) = Interval Perf Fourth
    MakeInterval (Pitch C Natural o1) (Pitch G Natural o1) = Interval Perf Fifth
    MakeInterval (Pitch C Natural o1) (Pitch A Natural o1) = Interval Maj Sixth
    MakeInterval (Pitch C Natural o1) (Pitch B Natural o1) = Interval Maj Seventh
    MakeInterval (Pitch C Natural o1) (Pitch C Natural o2) = Interval Perf Octave
    MakeInterval (Pitch C Natural o1) (Pitch pc2 Sharp o1) =
            Expand (MakeInterval (Pitch C Natural o1) (Pitch pc2 Natural o1))
    MakeInterval (Pitch C Natural o1) (Pitch pc2 Flat o1) =
            Shrink (MakeInterval (Pitch C Natural o1) (Pitch pc2 Natural o1))
    MakeInterval (Pitch C Flat o1) (Pitch pc2 acc o1) =
            Expand (MakeInterval (Pitch C Natural o1) (Pitch pc2 acc o1))
    MakeInterval (Pitch C Sharp o1) (Pitch pc2 acc o1) =
            Shrink (MakeInterval (Pitch C Natural o1) (Pitch pc2 acc o1))


    MakeInterval _ _ = TypeError (Text "Invalid interval.")

x :: Proxy (MakeInterval (Pitch C Sharp  Oct3) (Pitch G Flat Oct3))
x = undefined

y :: Proxy (OctSucc Oct8)
y = undefined

-- type family ValidPitch (p :: Pitch) :: Bool where
--     ValidPitch ('Pitch pc acc o) = TypeError (Text "Octave too high.")
--     -- ValidPitch _ = True
--
-- -- z :: Unit 4
-- -- z = Note (Proxy @('Pitch C Natural (OctSucc Oct8))) (Dur @Eighth)
--
-- data Unit :: Duration -> Type where
--     Note :: Ensure (ValidPitch p) => Proxy p -> Dur d -> Unit d
--     Rest :: Dur d -> Unit d
--
-- -- type family ValidDuration (d :: Duration) :: Bool where
-- --     ValidDuration Whole = True
-- --     ValidDuration (Half (Half (Half (Half (Half Whole))))) = TypeError (Text "Note duration must be longer than 1/32.")
-- --     ValidDuration (Half d) = ValidDuration d
-- --
-- -- -- type family ToInterval (pc1 :: PitchClass) (pc2 :: PitchClass) :: Interval where
-- -- --     ToInterval
-- --
type family Ensure (b :: Bool) :: Constraint where
    Ensure v = v ~ True
--
--
-- data Music :: Nat -> Type where
--     Prim :: Unit d -> Music d
--     (:-:) :: Music d1 -> Music d2 -> Music (d1 + d2)
--     (:|:) :: d1 ~ d2 => Music d1 -> Music d2 -> Music d1
--
--
-- -- m :: Music Half
-- -- m = Prim (Note (Proxy @('Pitch C Natural Oct3)) (Dur @Half)) :|: Prim (Rest (Dur @Half))
