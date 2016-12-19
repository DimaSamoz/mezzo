{-# LANGUAGE TypeInType, TypeOperators, TypeFamilies, GADTs,
    UndecidableInstances, ConstraintKinds, TypeApplications #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Prim
-- Description :  Mezzo type primitives
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Primitive types that make up the base for the Mezzo type model.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Prim
    (
    -- * Vectors and matrices
      Vector (..)
    , El (..)
    , IsEmpty
    , Head
    , Last
    , Matrix
    , From
    , Align
    , type (++)
    , type (**)
    , type (+*+)
    , type (+|+)
    , type (+-+)
    -- , Transpose
    -- * Booleans
    , If
    , Not
    , type (.&&.)
    , type (.||.)
    , type (.~.)
    -- * Constraints
    , Valid
    , Invalid
    , AllSatisfy
    , AllPairsSatisfy
    , SatisfiesAll
    , AllSatisfyAll
    ) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

infixr 5 :-
infixr 6 :*
infixr 6 **
infixl 4 ++
infixl 3 :--
infixl 4 +*+
infixl 4 +|+
infixl 4 +-+
infixl 3 .&&.
infixl 3 .||.
infixl 5 .~.

-------------------------------------------------------------------------------
-- Type-level vectors and matrices
-------------------------------------------------------------------------------

-- | A length-indexed vector.
data Vector :: Type -> Nat -> Type where
    Nil  :: Vector t 0
    (:-) :: El t l -> Vector t (n - l) -> Vector t n



type family (e :: t) :-- (v :: Vector t n) :: Vector t (n + 1) where
    el :-- (v :* n :- vs) =
        If (el .~. v)
           (v :* (S n) :- vs)
           (el ** 1 :- (v :* n) :- vs)

-- data Elem where
--     (:*) :: Type -> Nat -> Elem
--
-- data El (e :: Elem) where
--     El :: El e

data El :: Type -> Nat -> Type where
    (:*) :: t -> Rep n -> El t n

data Rep (n :: Nat) where
    R :: Rep n

type family S (r :: Rep n) :: Rep (n + 1) where
    S (R :: Rep n) = (R :: Rep (n + 1))

-- t :: (3 :-- ((5 :* (R :: Rep 3)) :- Nil)) ~ (3 :* (R :: Rep 1) :- (5 :* (R :: Rep 3)) :- Nil) => Int
-- t = 5
-- t :: (5 :-- ((5 :* (R :: Rep 3)) :- Nil)) ~ (5 :* (R :: Rep 1) :- (5 :* (R :: Rep 3)) :- Nil) => Int
-- t = 5

r ::Proxy ( (4 :* Quarter) :- Nil)
r = undefined

type Quarter = (R :: Rep 8)
type Once = (R :: Rep 1)
type family (v :: t) ** (d :: Nat) :: El t d where
     v ** d = v :* (R :: Rep d)

type family From (v :: t) (d :: Nat) :: Matrix t 1 d where
    From v d = ((v +*+ d) ** 1) :- Nil

-- | Get the first element of the vector.
type family Head (v :: Vector t n) :: t where
    Head Nil      = TypeError (Text "Vector has no head element.")
    Head (v :* _ :- _) = v

-- | Get the last element of the vector.
type family Last (v :: Vector t n) :: t where
    Last Nil        = TypeError (Text "Vector has no last element.")
    Last ((v :* _) :- Nil) = v
    Last (_ :- vs)  = Last vs

type family Tail (v :: Vector t n) :: Vector t (n - 1) where
    Tail Nil = TypeError (Text "Vector has no tail.")
    Tail (_ :- vs) = vs

type family IsEmpty (v :: Vector t n) :: Bool where
    IsEmpty Nil = True
    IsEmpty _   = False

-- | A dimension-indexed matrix.
type Matrix t p q = Vector (Vector t q) p

-- | Type-level vector appending.
type family (x :: Vector t n) ++ (y :: Vector t m) :: Vector t (n + m) where
    Nil       ++ y  = y
    (x :- xs) ++ y = x :- (xs ++ y)

-- | Repeat the specified type n times.
type family (a :: t) +*+ (n :: Nat) :: Vector t n where
    x +*+ 0 = Nil
    x +*+ n = (x ** n) :- Nil

-- | Horizontal concatenation of type-level matrices.
-- Places the first matrix to the left of the second.
type family (a :: Matrix t p q) +|+ (b :: Matrix t p r) :: Matrix t p (q + r) where
    Nil         +|+ Nil         = Nil
    (r1 :* _ :- rs1) +|+ (r2 :* _ :- rs2) = ((r1 ++ r2) ** 1) :- (rs1 +|+ rs2)

-- | Vertical concatenation of type-level matrices.
-- Places the first matrix on top of the second.
type family (a :: Matrix t p r) +-+ (b :: Matrix t q r) :: Matrix t (p + q) r where
    m1 +-+ m2 = ConcatPair (Align m1 m2)
    -- (r1 :* (R :: Rep k) :- rs1) +-+ (r2 :* (R :: Rep l) :- rs2) =
    --     If (k <=? )

type family ConcatPair (ms :: (Matrix t p r, Matrix t q r)) :: Matrix t (p + q) r where
    ConcatPair '(m1, m2) = m1 ++ m2

type family Align (a :: Matrix t p r) (b :: Matrix t q r) :: (Matrix t p r, Matrix t q r) where
    Align Nil m = '(Nil, m)
    Align m Nil = '(m, Nil)
    Align m1 m2 = '(FragmentMatByVec m1 (Head m2), FragmentMatByVec m2 (Head m1))

type family FragmentMatByVec (m :: Matrix t q p) (v :: Vector t p) :: Matrix t q p where
    FragmentMatByVec Nil _ = Nil
    FragmentMatByVec (r :* _ :- rs) v = (FragmentVecByVec r v) ** 1 :- FragmentMatByVec rs v

type family AddLine (v :: Vector t p) (m :: Matrix t q p) :: Matrix t (q + 1) p where
    AddLine v Nil = (v ** 1) :- Nil
    AddLine v (u :* (R :: Rep 1) :- us) = Join v u ++ Tail (AddLine v us) -- Rows in matrices are not repeated

type family FragmentVecByVec (v :: Vector t p) (u :: Vector t p) :: Vector t p where
    FragmentVecByVec Nil _ = Nil
    FragmentVecByVec (v :* (R :: Rep k) :- vs) (u :* (R :: Rep k) :- us) =
            v ** k :- (FragmentVecByVec vs us)
    FragmentVecByVec (v :* (R :: Rep k) :- vs) (u :* (R :: Rep l) :- us) =
        If (k <=? l)
            ((v ** k) :- (FragmentVecByVec vs (u ** (l - k) :- us)))
            ((v ** l) :- (FragmentVecByVec (v ** (k - l) :- vs) us))

-- type family ConsToPair (v :: El t1 d) (u :: El t2 d) (p :: (Vector t1 e, Vector t2 e))
--                     :: (Vector t1 (d + e), Vector t2 (d + e)) where
--     -- ConsToPair v u '(p1, Nil) = '(v :- p1, u :- p2)
--     ConsToPair v u '(p1, p2) = '(v :- p1, u :- p2)

type family Fst (p :: (t1, t2)) :: t1 where
    Fst '(v1, v2) = v1

type family Snd (p :: (t1, t2)) :: t2 where
    Snd '(v1, v2) = v2


type family Join (v :: Vector t p) (u :: Vector t p) :: Matrix t 2 p where
    -- Join Nil Nil = PairUp Nil Nil 0
    Join (v :* (R :: Rep k) :- Nil) (u :* (R :: Rep k) :- Nil) = PairUp v u k
    Join (v :* (R :: Rep k) :- vs) (u :* (R :: Rep k) :- us) =
        PairUp v u k +|+ Join vs us
    Join (v :* (R :: Rep k) :- vs) (u :* (R :: Rep l) :- us)=
        If (k <=? l)
            (PairUp v u k +|+ Join vs (u ** (l - k) :- us))
            (PairUp v u l +|+ Join (v ** (k - l) :- vs) us)

type family PairUp (a :: t) (b :: t) (n :: Nat) :: Matrix t 2 n where
    PairUp a b n = ((a ** n :- Nil) ** 1 :- (b ** n :- Nil) ** 1 :- Nil)

-- v :: Proxy (Join (Int ** 2 :- Nil) (Int ** 1 :- Int ** 1 :- Nil))
-- v = undefined
v :: (FragmentVecByVec (Int ** 3 :- Int ** 2 :- Nil) (Int ** 1 :- Int ** 4 :- Nil)) ~ (Int ** 1 :- Int ** 2 :- Int ** 2 :- Nil) => Int
v = 4


-- -- | Transposition of type-level matrices.
-- type family Transpose (a :: Matrix t p q) :: Matrix t q p where
--     Transpose Nil                = Nil
--     Transpose (Nil :- Nil)       = Nil
--     Transpose ((x :- xs) :- Nil) = (x :- Nil) :- (Transpose (xs :- Nil))
--     Transpose (row :- rows)      = (Transpose (row :- Nil)) +|+ Transpose rows

-------------------------------------------------------------------------------
-- Type-level booleans
-------------------------------------------------------------------------------

-- | Conditional expression at the type level.
type family If (b :: Bool) (t :: k) (e :: k) :: k where
    If True  t e = t
    If False t e = e

-- | Negation of type-level Booleans.
type family Not (a :: Bool) :: Bool where
    Not True = False
    Not False = True

-- | Conjunction of type-level Booleans.
type family (b1 :: Bool) .&&. (b2 :: Bool) :: Bool where
    b1 .&&. b2 = If b1 b2 False

-- | Disjunction of type-level Booleans.
type family (b1 :: Bool) .||. (b2 :: Bool) :: Bool where
    b1 .||. b2 = If b1 True b2

-- | Equality of types.
type family (a :: k) .~. (b :: k) :: Bool where
    a .~. a = True
    a .~. b = False

-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

-- | Valid base constraint.
type Valid = (() :: Constraint)

-- | Invalid base constraint.
type Invalid = True ~ False

-- | Create a new constraint which is valid only if every element in the given
-- vector satisfies the given unary constraint.
-- Analogue of 'map' for constraints and vectors.
type family AllSatisfy (c  :: a -> Constraint)
                       (xs :: Vector a n)
                           :: Constraint where
    AllSatisfy c Nil       = Valid
    AllSatisfy c ((x :* _) :- xs) = ((c x), AllSatisfy c xs)

-- | Create a new constraint which is valid only if every pair of elements in
-- the given vectors satisfy the given binary constraint.
-- Analogue of 'zipWith' for constraints and vectors.
type family AllPairsSatisfy (c  :: a -> b -> Constraint)
                            (xs :: Vector a n) (ys :: Vector b n)
                                :: Constraint where
    AllPairsSatisfy c Nil Nil             = Valid
    AllPairsSatisfy c ((x :* _) :- xs) ((y :* _) :- ys) = ((c x y), AllPairsSatisfy c xs ys)

-- | Create a new constraint which is valid only if the given value satisfies
-- every unary constraint in the given list.
type family SatisfiesAll (cs :: [a -> Constraint])
                         (xs :: a)
                             :: Constraint where
    SatisfiesAll '[] a      = Valid
    SatisfiesAll (c : cs) a = (c a, SatisfiesAll cs a)

-- | Create a new constraint which is valid only if every element in the given
-- vector satisfies every unary constraint in the given list.
type family AllSatisfyAll (c1 :: [a -> Constraint])
                          (xs :: Vector a n)
                              :: Constraint where
    AllSatisfyAll _ Nil = Valid
    AllSatisfyAll cs ((v :* _) :- vs) = (SatisfiesAll cs v, AllSatisfyAll cs vs)
