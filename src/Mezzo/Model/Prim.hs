{-# LANGUAGE TypeInType, TypeOperators, TypeFamilies, GADTs,
    UndecidableInstances, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

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
      Elem (..)
    , type (**)
    , Vector (..)
    , Head
    , Last
    , IsEmpty
    , Matrix
    , type (++)
    , type (+*+)
    , From
    , type (+|+)
    , type (+-+)
    , Align
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
import GHC.TypeLits

infixr 6 :*
infixr 6 **
infixr 5 :-
infixl 4 ++
infixl 4 +*+
infixl 4 +|+
infixl 4 +-+
infixl 3 .&&.
infixl 3 .||.
infixl 5 .~.

-------------------------------------------------------------------------------
-- Type-level vectors and matrices
-------------------------------------------------------------------------------

-- | Singleton type for the number of repetitions of an element.
data Times (n :: Nat) where
    T :: Times n

-- | An element of a "run-length encoded" vector, containing the value and
-- the number of repetitions
data Elem :: Type -> Nat -> Type where
    (:*) :: t -> Times n -> Elem t n

-- | Replicate a value the specified number of times to create a new 'Elem'.
type family (v :: t) ** (d :: Nat) :: Elem t d where
    v ** d = v :* (T :: Times d)

-- | A length-indexed vector, optimised for repetitions.
data Vector :: Type -> Nat -> Type where
    Nil  :: Vector t 0
    (:-) :: Elem t l -> Vector t (n - l) -> Vector t n

-- | Get the first element of the vector.
type family Head (v :: Vector t n) :: t where
    Head Nil           = TypeError (Text "Vector has no head element.")
    Head (v :* _ :- _) = v

-- | Get the last element of the vector.
type family Last (v :: Vector t n) :: t where
    Last Nil             = TypeError (Text "Vector has no last element.")
    Last (v :* _ :- Nil) = v
    Last (_ :- vs)       = Last vs

-- | Returns 'True' if the input vector is empty, 'False' otherwise.
type family IsEmpty (v :: Vector t n) :: Bool where
    IsEmpty Nil = True
    IsEmpty _   = False

-- | A dimension-indexed matrix.
type Matrix t p q = Vector (Vector t q) p

-- | Type-level vector appending.
type family (x :: Vector t n) ++ (y :: Vector t m) :: Vector t (n + m) where
    Nil       ++ y = y
    (x :- xs) ++ y = x :- (xs ++ y)

-- | Repeat the value the specified number of times to create a new 'Vector'.
type family (a :: t) +*+ (n :: Nat) :: Vector t n where
    x +*+ 0 = Nil
    x +*+ n = x ** n :- Nil

-- | Create a new one-row matrix from the specified type and duration.
type family From (v :: t) (d :: Nat) :: Matrix t 1 d where
    From v d = ((v +*+ d) ** 1) :- Nil

-- | Horizontal concatenation of type-level matrices.
-- Places the first matrix to the left of the second.
type family (a :: Matrix t p q) +|+ (b :: Matrix t p r) :: Matrix t p (q + r) where
    Nil              +|+ Nil              = Nil
    (r1 :* _ :- rs1) +|+ (r2 :* _ :- rs2) = ((r1 ++ r2) ** 1) :- (rs1 +|+ rs2)

-- | Vertical concatenation of type-level matrices.
-- Places the first matrix on top of the second.
type family (a :: Matrix t p r) +-+ (b :: Matrix t q r) :: Matrix t (p + q) r where
    m1 +-+ m2 = ConcatPair (Align m1 m2)

-- | Concatenates a type-level pair of vectors.
type family ConcatPair (vs :: (Vector t p, Vector t q)) :: Vector t (p + q) where
    ConcatPair '(v1, v2) = v1 ++ v2

-- | Vertically aligns two matrices by separating elements so that the element
-- boundaries line up.
type family Align (a :: Matrix t p r) (b :: Matrix t q r) :: (Matrix t p r, Matrix t q r) where
    Align Nil m = '(Nil, m)
    Align m Nil = '(m, Nil)
    Align m1 m2 = '(FragmentMatByVec m1 (Head m2), FragmentMatByVec m2 (Head m1))

-- | Fragments a matrix by a vector: all the element boundaries in the vector must
-- also appear in the fragmented matrix.
type family FragmentMatByVec (m :: Matrix t q p) (v :: Vector t p) :: Matrix t q p where
    FragmentMatByVec Nil _            = Nil
    FragmentMatByVec (r :* _ :- rs) v = (FragmentVecByVec r v) ** 1 :- FragmentMatByVec rs v

-- | Fragments a vector by another vector: all the element boundaries in the second
-- vector must also appear in the first.
type family FragmentVecByVec (v :: Vector t p) (u :: Vector t p) :: Vector t p where
    FragmentVecByVec Nil _ = Nil
    -- | If the lengths of the first element match up, they are not fragmented.
    FragmentVecByVec (v :* (T :: Times k) :- vs) (u :* (T :: Times k) :- us) =
            v ** k :- (FragmentVecByVec vs us)
    -- | If the length of the first element don't match up, we fragment the element
    -- by the shortest of the two lengths, and add the remainder as a separate element.
    FragmentVecByVec (v :* (T :: Times k) :- vs) (u :* (T :: Times l) :- us) =
        If (k <=? l)
            ((v ** k) :- (FragmentVecByVec vs (u ** (l - k) :- us)))
            ((v ** l) :- (FragmentVecByVec (v ** (k - l) :- vs) us))


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
    AllSatisfy c (x :* _ :- xs) = ((c x), AllSatisfy c xs)

-- | Create a new constraint which is valid only if every pair of elements in
-- the given vectors satisfy the given binary constraint.
-- Analogue of 'zipWith' for constraints and vectors.
type family AllPairsSatisfy (c  :: a -> b -> Constraint)
                            (xs :: Vector a n) (ys :: Vector b n)
                                :: Constraint where
    AllPairsSatisfy c Nil Nil                       = Valid
    AllPairsSatisfy c (x :* _ :- xs) (y :* _ :- ys) = ((c x y), AllPairsSatisfy c xs ys)

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
    AllSatisfyAll _ Nil             = Valid
    AllSatisfyAll cs (v :* _ :- vs) = (SatisfiesAll cs v, AllSatisfyAll cs vs)
