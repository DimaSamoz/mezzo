{-# LANGUAGE TypeInType, GADTs, TypeFamilies, TypeOperators, FlexibleContexts, MultiParamTypeClasses,
    TypeApplications, UndecidableInstances, FlexibleInstances, RankNTypes, ConstraintKinds #-}
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

-- data Unit :: Duration -> Type where
--     NoteU :: Note p acc o d -> Unit d
--     RestU :: Rest d -> Unit d

n :: Note C Natural Oct3 (Dotted Half)
n = Note Pitch Dur

class Unit u
instance Unit (Note pc acc oct dur)
instance Unit (Rest dur)

type family HalfOf (d :: Duration) :: Duration where
    HalfOf ThirtySecond = TypeError (Text "Note duration can't be smaller than 1/32")
    HalfOf Sixteenth = ThirtySecond
    HalfOf Eighth = Sixteenth
    HalfOf Quarter = Eighth
    HalfOf Half = Quarter
    HalfOf Whole = Half

type family Dotted (d :: Duration) :: Duration where
    Dotted d = d + HalfOf d

data Vec :: Type -> Nat -> Type where
    Nil :: Vec a 0
    (:-) :: a -> Vec a (n - 1) -> Vec a n

type family (x :: Vec a n) ++ (y :: Vec a m) :: Vec a (n + m) where
    Nil ++ y = y
    (h :- t) ++ y = h :- (t ++ y)

type Matrix a p q = Vec (Vec a q) p

type family (a :: Matrix t p q) +-+ (b :: Matrix t p r) :: Matrix t p (q + r) where
    Nil         +-+ Nil         = Nil
    (r1 :- rs1) +-+ (r2 :- rs2) = (r1 ++ r2) :- (rs1 +-+ rs2)

type family (a :: Matrix t p r) +|+ (b :: Matrix t q r) :: Matrix t (p + q) r where
    m1 +|+ m2 = m1 ++ m2

-- data UnitP (u :: Unit d) = UnitP

-- m :: Proxy (((1 :- (2 :- Nil)) :- ((3 :- (4 :- Nil)) :- Nil)) +-+ ((6 :- Nil) :- ((6 :- Nil) :- Nil)))
-- m = undefined

type family PitchToNat n :: Nat where
    PitchToNat (Pitch C Natural Oct_1) = 0
    PitchToNat (Pitch C Sharp Oct_1) = 1
    PitchToNat (Pitch D Flat Oct_1) = 1
    PitchToNat p = 1 + (PitchToNat (HalfStepDown p))

type family NatToPitch (n :: Nat) where
    NatToPitch 0 = Pitch C Natural Oct_1
    NatToPitch 1 = Pitch C Sharp Oct_1
    NatToPitch n = HalfStepUp (NatToPitch (n - 1))

test :: Proxy (PitchToNat (Pitch C Natural Oct4))
test = Proxy @60

test2 :: Proxy (NatToPitch 61)
test2 = Proxy @(Pitch C Sharp Oct4)

data Music :: forall u p q. Matrix u p q -> Type where
    Prim :: Unit u => u -> Music ((u :- Nil) :- Nil)
    (:|:) :: Music m1 -> Music m2 -> Music (m1 +|+ m2)
    (:-:) :: AllValidLeaps m1 m2 => Music m1 -> Music m2 -> Music (m1 +-+ m2)

-- type family ValidLeap (v1 :: Vec a n) (v2 :: Vec a m) :: Constraint where
--     ValidLeap Nil _ = Valid
--     ValidLeap _ Nil = Valid
--     ValidLeap (v1 :- Nil) (v2 :- vs2) = ValidNoteLeap v1 v2
--     ValidLeap (_ :- vs1) vs2 = ValidLeap vs1 vs2

class ValidLeap a b
instance ValidLeap Nil a
instance ValidLeap a Nil
instance ValidNoteLeap v1 v2 => ValidLeap (v1 :- Nil) (v2 :- vs2)
instance {-# OVERLAPPABLE #-}  ValidLeap vs1 vs2 => ValidLeap (v :- vs1) vs2

class ValidNoteLeap n1 n2
instance ValidNoteLeap (Rest d) a
instance ValidNoteLeap a (Rest d)
instance ValidMelInterval (MakeInterval n1 n2) => ValidNoteLeap n1 n2


-- type family ValidNoteLeap n1 n2 :: Constraint where
--     ValidNoteLeap (Rest d) _ = Valid
--     ValidNoteLeap _ (Rest d) = Valid
--     ValidNoteLeap n1 n2 = ValidMelInterval (MakeInterval n1 n2)

class AllValidLeaps m1 m2
instance AllValidLeaps Nil Nil
instance (ValidLeap row1 row2, AllValidLeaps rest1 rest2) => AllValidLeaps (row1 :- rest1) (row2 :- rest2)

-- type family AllValidLeaps (m1 :: Matrix a p r) (m2 :: Matrix a p q) :: Bool where
--     AllValidLeaps Nil Nil = True
--     AllValidLeaps (row1 :- rest1) (row2 :- rest2) = ValidLeap row1 row2 `And` AllValidLeaps rest1 rest2

type family And (b1 :: Bool) (b2 :: Bool) :: Bool where
    And b1 b2 = If (b1) (b2) False

type family Or (b1 :: Bool) (b2 :: Bool) :: Bool where
    Or False False = False
    Or _ _ = True

class ValidMelInterval i
instance TypeError (Text "Augmented melodic intervals are not permitted.") => ValidMelInterval (Interval Aug a)
instance TypeError (Text "Augmented melodic intervals are not permitted.") => ValidMelInterval (Interval Dim a)
instance TypeError (Text "Seventh intervals are not permitted in melody.") => ValidMelInterval (Interval a Seventh)
instance ValidMelInterval (Interval a b)

-- type family ValidMelInterval i :: Constraint where
--     ValidMelInterval (Interval Aug _) = TypeError (Text "Augmented melodic intervals are not permitted.")
--     ValidMelInterval (Interval Dim _) = TypeError (Text "Diminished melodic intervals are not permitted.")
--     ValidMelInterval (Interval _ Seventh) = TypeError (Text "Seventh intervals are not permitted in melody.")
--     ValidMelInterval (Interval _ _) = Valid

invLeap :: Proxy (ValidLeap (Note C Natural Oct3 Half :- Nil) (Note G Flat Oct3 Half :- Nil))
invLeap = undefined

type family OrdPair p1 p2 :: (Nat, Nat) where
    OrdPair p1 p2 =
        If  (PitchToNat p1 <=? PitchToNat p2)
            '((PitchToNat p1), (PitchToNat p2))
            '((PitchToNat p2), (PitchToNat p1))

-- type Leap n1 n2 = MakeInterval

primR :: Music (((Rest Half) :- Nil) :- Nil)
primR = Prim (Rest (Dur @Half))

primN :: Music ((Note F Sharp Oct3 Half :- Nil) :- Nil)
primN = Prim (Note (Pitch @F @Sharp @Oct3) (Dur @Half))

primMel :: Music ((Rest Half :- Nil) :- (Nil ++ ((Note F Sharp Oct3 Half :- Nil) :- Nil)))
primMel = primR :|: primN

bbt :: Proxy (MakeInterval (Pitch F Natural Oct3) (Pitch B Natural Oct3))
bbt = undefined

bb =
    (Prim (Note (Pitch @C @Natural @Oct3) (Dur @Eighth))) :-:
    (Prim (Note (Pitch @E @Natural @Oct3) (Dur @Eighth)))

bt = ( bb) :|: bb


data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave deriving (Eq, Show, Ord)

data IntervalClass = Maj | Perf | Min | Aug | Dim deriving (Eq, Show)

data Interval :: IntervalClass -> IntervalSize -> Type where
    Interval :: Interval ic is

type family Shrink i where
    Shrink (Interval Perf Unison) = TypeError (Text "Can't diminish unisons.")
    Shrink (Interval Perf is) = Interval Dim is
    Shrink (Interval Min is) = Interval Dim is
    Shrink (Interval Maj is) = Interval Min is
    Shrink (Interval Aug Unison) = Interval Perf Unison
    Shrink (Interval Aug Fourth) = Interval Perf Fourth
    Shrink (Interval Aug Fifth) = Interval Perf Fifth
    Shrink (Interval Aug Octave) = Interval Perf Octave
    Shrink (Interval Aug is) = Interval Maj is
    Shrink (Interval Dim Unison) = TypeError (Text "Can't diminish unisons.")
    Shrink (Interval Dim Second) = TypeError (Text "Can't diminish unisons.")
    Shrink (Interval Dim Fifth) = Interval Perf Fourth
    Shrink (Interval Dim Sixth) = Interval Dim Fifth
    Shrink (Interval Dim is) = Interval Min (IntSizePred is)

type family Expand i where
    Expand (Interval Perf Octave) = Interval Aug Octave
    Expand (Interval Perf is) = Interval Aug is
    Expand (Interval Maj is) = Interval Aug is
    Expand (Interval Min is) = Interval Maj is
    Expand (Interval Dim Unison) = TypeError (Text "Can't diminish unisons.")
    Expand (Interval Dim Fourth) = Interval Perf Fourth
    Expand (Interval Dim Fifth) = Interval Perf Fifth
    Expand (Interval Dim Octave) = Interval Perf Octave
    Expand (Interval Dim is) = Interval Min is
    Expand (Interval Aug Third) = Interval Aug Fourth
    Expand (Interval Aug Fourth) = Interval Perf Fifth
    Expand (Interval Aug Seventh) = Interval Aug Octave
    Expand (Interval Aug Octave) = TypeError (Text "Compound intervals are not supported.")
    Expand (Interval Aug is) = Interval Maj (IntSizeSucc is)

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
    HalfStepUp (Pitch pc Sharp o) = Pitch (ClassSucc pc) Natural o

type family HalfStepDown p where
    HalfStepDown (Pitch C acc o) = Pitch B acc (OctPred o)
    HalfStepDown (Pitch F acc o) = Pitch E acc o
    HalfStepDown (Pitch pc Flat o) = Pitch (ClassPred pc) Natural o
    HalfStepDown (Pitch pc Natural o) = Pitch pc Flat o
    HalfStepDown (Pitch pc Sharp o) = Pitch pc Natural o

type family If (b :: Bool) (t :: k) (e :: k) :: k where
    If True t e = t
    If False t e = e
    If er _ _ = er

type family a .~. b :: Bool where
    a .~. a = True
    a .~. b = False
infixl 5 .~.

type family MakeInterval p1 p2 where
    MakeInterval (Note pc1 acc1 oct1 dur1) (Note pc2 acc2 oct2 dur2) =
        MakeInterval (Pitch pc1 acc1 oct1) (Pitch pc2 acc2 oct2)
    MakeInterval p1 p2 =
        If  (PitchToNat p1 <=? PitchToNat p2)
            (MakeIntervalOrd p1 p2)
            (MakeIntervalOrd p2 p1)

type family MakeIntervalOrd p1 p2 where
    MakeIntervalOrd p p = Interval Perf Unison
    MakeIntervalOrd (Pitch C Natural o) (Pitch D Natural o) = Interval Maj Second
    MakeIntervalOrd (Pitch C Natural o) (Pitch E Natural o) = Interval Maj Third
    MakeIntervalOrd (Pitch C Natural o) (Pitch F Natural o) = Interval Perf Fourth
    MakeIntervalOrd (Pitch C Natural o) (Pitch G Natural o) = Interval Perf Fifth
    MakeIntervalOrd (Pitch C Natural o) (Pitch A Natural o) = Interval Maj Sixth
    MakeIntervalOrd (Pitch C Natural o) (Pitch B Natural o) = Interval Maj Seventh
    MakeIntervalOrd (Pitch C Natural o) (Pitch C Natural o2) =
            If (OctSucc o .~. o2) (Interval Perf Octave) (TypeError (Text "Compound intervals are not supported."))
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Sharp o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Flat o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch C Flat o) (Pitch pc2 acc o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch C Sharp o) (Pitch pc2 acc o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch pc1 acc1 o) (Pitch pc2 acc2 o) =
            MakeIntervalOrd (Pitch (ClassPred pc1) acc1 o) (Pitch (ClassPred pc2) acc2 o)
    MakeIntervalOrd (Pitch pc1 acc1 o1) (Pitch pc2 acc2 o2) =
            If  (OctSucc o1 .~. o2)
                (MakeIntervalOrd (HalfStepDown (Pitch pc1 acc1 o1)) (HalfStepDown (Pitch pc2 acc2 o2)))
                (TypeError (Text "Compound intervals are not supported."))
    MakeIntervalOrd (Note pc1 acc1 o1 d1) (Note pc2 acc2 o2 d2) = MakeIntervalOrd (Pitch pc1 acc1 o1) (Pitch pc2 acc2 o2)
    MakeIntervalOrd _ _ = TypeError (Text "Invalid interval.")

x :: MakeInterval (Pitch E Natural Oct3) (Pitch C Natural Oct_1)
x = undefined

-- class ValidHarmInterval i
-- instance TypeError (Text "Can't have minor seconds in chords.") => ValidHarmInterval (Interval Aug Unison)
-- instance TypeError (Text "Can't have minor seconds in chords.") => ValidHarmInterval (Interval Min Second)
-- instance TypeError (Text "Can't have major sevenths in chords.") => ValidHarmInterval (Interval Maj Seventh)
-- instance TypeError (Text "Can't have major sevenths in chords.") => ValidHarmInterval (Interval Dim Octave)
-- instance {-# OVERLAPPABLE #-} ValidHarmInterval i


type Invalid = True ~ False
type Valid = (() :: Constraint)

type family ValidHarmInterval i :: Bool where
    ValidHarmInterval (Interval Aug Unison) = TypeError (Text "Can't have minor seconds in chords.")
    ValidHarmInterval (Interval Min Second) = TypeError (Text "Can't have minor seconds in chords.")
    ValidHarmInterval (Interval Maj Seventh) = TypeError (Text "Can't have major sevenths in chords.")
    ValidHarmInterval (Interval Dim Octave) = TypeError (Text "Can't have major sevenths in chords.")
    ValidHarmInterval i = True

data ValidTest where
    V :: Ensure (ValidHarmInterval (MakeInterval (Note pc1 a1 o1 d1) (Note pc2 a2 o2 d2)))
            => Note pc1 a1 o1 d1 -> Note pc2 a2 o2 d2 -> ValidTest

as :: ValidTest
as = V n1 n2

n1 :: Note B Flat Oct3 Half
n1 = undefined

n2 :: Note C Natural Oct3 Half
n2 = undefined

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
