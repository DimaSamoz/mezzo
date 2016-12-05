{-# LANGUAGE TypeInType, GADTs, TypeFamilies, TypeOperators, FlexibleContexts, MultiParamTypeClasses,
    TypeApplications, UndecidableInstances, FlexibleInstances, RankNTypes, ConstraintKinds, PartialTypeSignatures #-}
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

type Duration = Nat

data PCS (pc :: PitchClass) where
    PCS :: PCS pc

data AccS (acc :: Accidental) where
    AccS :: AccS acc

data OctS (oct :: Oct) where
    OctS :: OctS oct

data Dur (a :: Duration) where
    Dur :: Dur a

type Whole = 32
type Half = 16
type Quarter = 8
type Eighth = 4
type Sixteenth = 2
type ThirtySecond = 1



-- data Pitch :: PitchClass -> Accidental -> Oct -> Type where
--     Pitch :: Pitch pc acc o

data PitchType where
    Pitch :: PitchClass -> Accidental -> Oct -> PitchType
    Silence :: PitchType

data PitchS (p :: PitchType) where
    PitchS :: PitchS p

-- data Note :: PitchClass -> Accidental -> Oct -> Duration -> Type where
--     Note :: Pitch pc acc o -> Dur d -> Note pc acc o d
--
-- data Rest :: Duration -> Type where
--     Rest :: Dur d -> Rest d

-- data Unit :: Duration -> Type where
--     NoteU :: Note p acc o d -> Unit d
--     RestU :: Rest d -> Unit d

-- n :: Music _
n = Note (PCS @C) (AccS @Natural) (OctS @Oct3) (Dur @Sixteenth)

r = Rest (Dur @Eighth)

-- class Unit u
-- instance Unit (Note pc acc oct dur)
-- instance Unit (Rest dur)

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
infixr 5 :-

type family (x :: Vec a n) ++ (y :: Vec a m) :: Vec a (n + m) where
    Nil ++ y = y
    (h :- t) ++ y = h :- (t ++ y)

type Matrix a p q = Vec (Vec a q) p

type family (a :: Matrix t p q) +-+ (b :: Matrix t p r) :: Matrix t p (q + r) where
    Nil         +-+ Nil         = Nil
    (r1 :- rs1) +-+ (r2 :- rs2) = (r1 ++ r2) :- (rs1 +-+ rs2)

type family (a :: Matrix t p r) +|+ (b :: Matrix t q r) :: Matrix t (p + q) r where
    m1 +|+ m2 = m1 ++ m2

type family Transpose (a :: Matrix t p q) :: Matrix t q p where
    Transpose Nil = Nil
    Transpose (Nil :- Nil) = Nil
    Transpose ((x :- xs) :- Nil) = (x :- Nil) :- (Transpose (xs :- Nil))
    Transpose (row :- rows) = (Transpose (row :- Nil)) +-+ Transpose rows

matT :: Proxy (Transpose ((1 :- 2 :- Nil) :- (3 :- 4 :- Nil) :- Nil))
matT = undefined

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

type family Repeat (a :: t) (n :: Nat) :: Vec t n where
    Repeat x 0 = Nil
    Repeat x n = x :- (Repeat x (n-1))

type family NoteToVec (p :: PitchType) (d :: Duration) :: Vec PitchType d where
    NoteToVec pitch dur = Repeat pitch dur

type family RestToVec (d :: Duration) :: Vec PitchType d where
    RestToVec dur = Repeat Silence dur

data Music :: forall u p q. Matrix u p q -> Type where
    Note :: PCS pc -> AccS acc -> OctS oct -> Dur d -> Music (NoteToVec (Pitch pc acc oct) d :- Nil)
    Rest :: Dur d -> Music (RestToVec d :- Nil)
    (:|:) :: ValidParComp m1 m2 => Music m1 -> Music m2 -> Music (m1 +|+ m2)
    (:-:) :: ValidSeqComp m1 m2 => Music m1 -> Music m2 -> Music (m1 +-+ m2)

type Composition = forall m. Music m
data Comp = forall m. Comp (Music m)

type ValidParComp m1 m2 = (AllSatisfy ValidIntervals (Transpose (m1 +|+ m2)))

type ValidSeqComp m1 m2 = (AllValidLeaps m1 m2)


class ValidMelInterval i
instance {-# OVERLAPPING #-}    ValidMelInterval (Interval Aug Unison)
instance {-# OVERLAPS #-}       TypeError (Text "Augmented melodic intervals are not permitted.") => ValidMelInterval (Interval Aug a)
instance {-# OVERLAPS #-}       TypeError (Text "Augmented melodic intervals are not permitted.") => ValidMelInterval (Interval Dim a)
instance {-# OVERLAPPING #-}    TypeError (Text "Seventh intervals are not permitted in melody.") => ValidMelInterval (Interval a Seventh)
instance {-# OVERLAPPABLE #-}   ValidMelInterval i

class ValidNoteLeap n1 n2
instance {-# OVERLAPPING #-} ValidNoteLeap (Silence) (Pitch pc acc oct)
instance {-# OVERLAPPING #-} ValidNoteLeap (Pitch pc acc oct) (Silence)
instance {-# OVERLAPPABLE #-} ValidMelInterval (MakeInterval a b) => ValidNoteLeap a b

class ValidLeap (a :: Vec t n) (b :: Vec t m)
instance {-# OVERLAPPING #-} ValidLeap Nil a
instance {-# OVERLAPPING #-} ValidLeap a Nil
instance {-# OVERLAPS #-} ValidNoteLeap v1 v2 => ValidLeap (v1 :- Nil) (v2 :- vs2)
instance {-# OVERLAPPABLE #-} ValidLeap vs1 vs2 => ValidLeap (v :- vs1) vs2

class AllValidLeaps (m1 :: Matrix t p q) (m2 :: Matrix t p r)
instance {-# OVERLAPPING #-} AllValidLeaps Nil Nil
instance {-# OVERLAPPABLE #-} (ValidLeap row1 row2, AllValidLeaps rest1 rest2) => AllValidLeaps (row1 :- rest1) (row2 :- rest2)




class ValidHarmInterval i
instance {-# OVERLAPPING #-} TypeError (Text "Can't have minor seconds in chords.") => ValidHarmInterval (Interval Aug Unison)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have minor seconds in chords.") => ValidHarmInterval (Interval Min Second)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have major sevenths in chords.") => ValidHarmInterval (Interval Maj Seventh)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have major sevenths in chords.") => ValidHarmInterval (Interval Dim Octave)
instance {-# OVERLAPPING #-} TypeError (Text "Can't have augmented octaves in chords.") => ValidHarmInterval (Interval Aug Octave)
instance {-# OVERLAPPABLE #-} ValidHarmInterval i
-- instance (Interval Aug Unison ~ i, TypeError (Text "Can't have minor seconds in chords.")) => ValidHarmInterval i

class ValidNoteInterval n1 n2
instance {-# OVERLAPPING #-} ValidNoteInterval (Silence) (Silence)
instance {-# OVERLAPPING #-} ValidNoteInterval (Pitch pc acc oct) (Silence)
instance {-# OVERLAPPING #-} ValidNoteInterval (Silence) (Pitch pc acc oct)
instance {-# OVERLAPPABLE #-} ValidHarmInterval (MakeInterval a b) => ValidNoteInterval a b

type family AllPairsSatisfy (c :: a -> a -> Constraint) (e :: a) (xs :: Vec a n) :: Constraint where
    AllPairsSatisfy c e xs = AllSatisfy (c e) xs

type family AllSatisfy (c :: a -> Constraint) (xs :: Vec a n) :: Constraint where
    AllSatisfy c Nil = Valid
    AllSatisfy c (x :- xs) = ((c x), AllSatisfy c xs)

class ValidIntervals (m :: Vec t n)
instance {-# OVERLAPPING #-} ValidIntervals Nil
instance {-# OVERLAPPING #-} ValidIntervals (n :- Nil)
instance {-# OVERLAPPING #-} ValidNoteInterval n1 n2 => ValidIntervals (n1 :- n2 :- Nil)
instance {-# OVERLAPPABLE #-} (AllPairsSatisfy ValidNoteInterval n ns, ValidIntervals ns) => ValidIntervals (n :- ns)



type family Head (v :: Vec a n) :: a where
    Head (x :- xs) = x

type family Tail (v :: Vec a n) :: Vec a (n - 1) where
    Tail (x :- xs) = xs

-- type family ValidNoteLeap n1 n2 :: Constraint where
--     ValidNoteLeap (Rest d) _ = Valid
--     ValidNoteLeap _ (Rest d) = Valid
--     ValidNoteLeap n1 n2 = ValidMelInterval (MakeInterval n1 n2)

-- type family AllValidLeaps (m1 :: Matrix a p r) (m2 :: Matrix a p q) :: Bool where
--     AllValidLeaps Nil Nil = True
--     AllValidLeaps (row1 :- rest1) (row2 :- rest2) = ValidLeap row1 row2 `And` AllValidLeaps rest1 rest2

type family (b1 :: Bool) .&&. (b2 :: Bool) :: Bool where
    b1 .&&. b2 = If (b1) (b2) False
infixl 3 .&&.

type family (b1 :: Bool) .||. (b2 :: Bool) :: Bool where
    False .||. False = False
    _ .||.  _ = True
infixl 3 .||.

bb :: Comp
bb = Comp $
    ((Note (PCS @G) (AccS @Natural) (OctS @Oct3) (Dur @Eighth)) :|:
    (Note (PCS @B) (AccS @Natural) (OctS @Oct3) (Dur @Eighth)) :|:
    (Note (PCS @D) (AccS @Natural) (OctS @Oct4) (Dur @Eighth)) :|:
    (Note (PCS @F) (AccS @Natural) (OctS @Oct4) (Dur @Eighth)))
    :-:
    ((Note (PCS @G) (AccS @Natural) (OctS @Oct3) (Dur @Eighth)) :|:
    (Note (PCS @C) (AccS @Natural) (OctS @Oct4) (Dur @Eighth)) :|:
    (Note (PCS @E) (AccS @Natural) (OctS @Oct5) (Dur @Eighth)) :|:
    (Rest (Dur @Sixteenth) :-: Rest (Dur @Sixteenth)))
-- bt = ( bb) :|: bb


data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave deriving (Eq, Show, Ord)

data IntervalClass = Maj | Perf | Min | Aug | Dim deriving (Eq, Show)

data IntervalType where
    Interval :: IntervalClass -> IntervalSize -> IntervalType
    Compound :: IntervalType

type family Shrink (i :: IntervalType) :: IntervalType where
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
    Shrink (Compound) = Compound

type family Expand (i :: IntervalType) :: IntervalType where
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
    Expand (Interval Aug Octave) = Compound
    Expand (Interval Aug is) = Interval Maj (IntSizeSucc is)
    Expand (Compound) = Compound

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
    -- MakeInterval (Note pc1 acc1 oct1 dur1) (Note pc2 acc2 oct2 dur2) =
    --     MakeInterval (Pitch pc1 acc1 oct1) (Pitch pc2 acc2 oct2)
    MakeInterval p1 p2 =
        If  (PitchToNat p1 <=? PitchToNat p2)
            (MakeIntervalOrd p1 p2)
            (MakeIntervalOrd p2 p1)

type family MakeIntervalOrd p1 p2 :: IntervalType where
    MakeIntervalOrd p p = Interval Perf Unison
    MakeIntervalOrd (Pitch C acc o) (Pitch D acc o) = Interval Maj Second
    MakeIntervalOrd (Pitch C acc o) (Pitch E acc o) = Interval Maj Third
    MakeIntervalOrd (Pitch C acc o) (Pitch F acc o) = Interval Perf Fourth
    MakeIntervalOrd (Pitch C acc o) (Pitch G acc o) = Interval Perf Fifth
    MakeIntervalOrd (Pitch C acc o) (Pitch A acc o) = Interval Maj Sixth
    MakeIntervalOrd (Pitch C acc o) (Pitch B acc o) = Interval Maj Seventh
    MakeIntervalOrd (Pitch C acc o) (Pitch C acc o2) =
            If (OctSucc o .~. o2) (Interval Perf Octave) (Compound)
    MakeIntervalOrd (Pitch C Natural o) (Pitch C Sharp o2) =
            If (OctSucc o .~. o2) (Interval Aug Octave) (Compound)
    MakeIntervalOrd (Pitch C Flat o) (Pitch C Natural o2) =
            If (OctSucc o .~. o2) (Interval Aug Octave) (Compound)
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Sharp o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Flat o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 Natural o))
    MakeIntervalOrd (Pitch C Flat o) (Pitch pc2 acc o) =
            Expand (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch C Sharp o) (Pitch pc2 acc o) =
            Shrink (MakeIntervalOrd (Pitch C Natural o) (Pitch pc2 acc o))
    MakeIntervalOrd (Pitch pc1 acc1 o1) (Pitch pc2 acc2 o2) =
            If  (o1 .~. o2 .||. OctSucc o1 .~. o2)
                (MakeIntervalOrd (HalfStepDown (Pitch pc1 acc1 o1)) (HalfStepDown (Pitch pc2 acc2 o2)))
                (Compound)
    -- MakeIntervalOrd (Note pc1 acc1 o1 d1) (Note pc2 acc2 o2 d2) = MakeIntervalOrd (Pitch pc1 acc1 o1) (Pitch pc2 acc2 o2)
    MakeIntervalOrd _ _ = TypeError (Text "Invalid interval.")

x :: Proxy (MakeInterval (Pitch F Natural Oct3) (Pitch B Natural Oct3))
x = undefined



type Invalid = True ~ False
type Valid = (() :: Constraint)

-- type family ValidHarmInterval i :: Bool where
--     ValidHarmInterval (Interval Aug Unison) = TypeError (Text "Can't have minor seconds in chords.")
--     ValidHarmInterval (Interval Min Second) = TypeError (Text "Can't have minor seconds in chords.")
--     ValidHarmInterval (Interval Maj Seventh) = TypeError (Text "Can't have major sevenths in chords.")
--     ValidHarmInterval (Interval Dim Octave) = TypeError (Text "Can't have major sevenths in chords.")
--     ValidHarmInterval i = True



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
