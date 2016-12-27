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
      Vector (..)
    , Elem (..)
    , type (**)
    , OptVector (..)
    , Head
    , Head'
    , Last
    , Tail'
    , Matrix
    , type (++)
    , type (:-|)
    , type (+*+)
    , From
    , type (+|+)
    , type (+-+)
    , Align
    , VectorToColMatrix
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
    , AllPairsSatisfy'
    , SatisfiesAll
    , AllSatisfyAll
    ) where

import Data.Kind
import GHC.TypeLits

infixr 6 :*
infixr 6 **
infixr 5 :-
infixr 5 :--
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

-- | Simple length-indexed vector.
data Vector :: Type -> Nat -> Type where
    None :: Vector t 0
    (:--) :: t -> Vector t (n - 1) -> Vector t n

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
data OptVector :: Type -> Nat -> Type where
    End  :: OptVector t 0
    (:-) :: Elem t l -> OptVector t (n - l) -> OptVector t n

-- | Get the first element of an optimised vector.
type family Head (v :: OptVector t n) :: t where
    Head End           = TypeError (Text "Vector has no head element.")
    Head (v :* _ :- _) = v

-- | Get the first element of a simple vector.
type family Head' (v :: Vector t n) :: t where
    Head' None      = TypeError (Text "Vector has no head element.")
    Head' (v :-- _) = v

-- | Get the last element of the vector.
type family Last (v :: OptVector t n) :: t where
    Last End             = TypeError (Text "Vector has no last element.")
    Last (v :* _ :- End) = v
    Last (_ :- vs)       = Last vs

type family Tail' (v :: Vector t n) :: Vector t (n - 1) where
    Tail' None = TypeError (Text "Vector has no tail.")
    Tail' (_ :-- vs) = vs

-- | A dimension-indexed matrix.
type Matrix t p q = Vector (OptVector t q) p

-- | Append two optimised vectors.
type family (x :: OptVector t n) ++ (y :: OptVector t m) :: OptVector t (n + m) where
    End       ++ ys = ys
    (x :- xs) ++ ys = x :- (xs ++ ys)

-- | Append two simple vectors.
type family (x :: Vector t n) ++. (y :: Vector t m) :: Vector t (n + m) where
    None       ++. ys = ys
    (x :-- xs) ++. ys = x :-- (xs ++. ys)

-- | Add an element to the end of a simple vector.
type family (v :: Vector t (n - 1)) :-| (e :: t) :: Vector t n where
    v :-| e = v ++. (e :-- None)

-- | Repeat the value the specified number of times to create a new 'OptVector'.
type family (a :: t) +*+ (n :: Nat) :: OptVector t n where
    x +*+ 0 = End
    x +*+ n = x ** n :- End

-- | Create a new one-row matrix from the specified type and duration.
type family From (v :: t) (d :: Nat) :: Matrix t 1 d where
    From v d = (v +*+ d) :-- None

-- | Horizontal concatenation of type-level matrices.
-- Places the first matrix to the left of the second.
type family (a :: Matrix t p q) +|+ (b :: Matrix t p r) :: Matrix t p (q + r) where
    None         +|+ None         = None
    (r1 :-- rs1) +|+ (r2 :-- rs2) = (r1 ++ r2) :-- (rs1 +|+ rs2)

-- | Vertical concatenation of type-level matrices.
-- Places the first matrix on top of the second.
type family (a :: Matrix t p r) +-+ (b :: Matrix t q r) :: Matrix t (p + q) r where
    m1 +-+ m2 = ConcatPair (Align m1 m2)

-- | Concatenates a type-level pair of vectors.
type family ConcatPair (vs :: (Vector t p, Vector t q)) :: Vector t (p + q) where
    ConcatPair '(v1, v2) = v1 ++. v2

-- | Vertically aligns two matrices by separating elements so that the element
-- boundaries line up.
type family Align (a :: Matrix t p r) (b :: Matrix t q r) :: (Matrix t p r, Matrix t q r) where
    Align None m = '(None, m)
    Align m None = '(m, None)
    Align (r1 :-- rs1) (r2 :-- rs2) =
            '(FragmentMatByVec (r1 :-- rs1) r2, FragmentMatByVec (r2 :-- rs2) r1)

-- | Fragments a matrix by a vector: all the element boundaries in the vector must
-- also appear in the fragmented matrix.
type family FragmentMatByVec (m :: Matrix t q p) (v :: OptVector t p) :: Matrix t q p where
    FragmentMatByVec None       _ = None
    FragmentMatByVec (r :-- rs) v = FragmentVecByVec r v :-- FragmentMatByVec rs v

-- | Fragments a vector by another vector: all the element boundaries in the second
-- vector must also appear in the first.
type family FragmentVecByVec (v :: OptVector t p) (u :: OptVector t p) :: OptVector t p where
    FragmentVecByVec End _ = End
    -- If the lengths of the first element match up, they are not fragmented.
    FragmentVecByVec (v :* (T :: Times k) :- vs) (u :* (T :: Times k) :- us) =
            v ** k :- (FragmentVecByVec vs us)
    -- If the lengths of the first elements don't match up, we fragment the element
    -- by the shortest of the two lengths, and add the remainder as a separate element.
    FragmentVecByVec (v :* (T :: Times k) :- vs) (u :* (T :: Times l) :- us) =
        If (k <=? l)
            ((v ** k) :- (FragmentVecByVec vs (u ** (l - k) :- us)))
            ((v ** l) :- (FragmentVecByVec (v ** (k - l) :- vs) us))

-- | Convert a simple vector to a column matrix.
type family VectorToColMatrix (v :: Vector t n) (l :: Nat) :: Matrix t n l where
    VectorToColMatrix None _ = None
    VectorToColMatrix (v :-- vs) l = ((v ** l) :- End) :-- (VectorToColMatrix vs l)

-------------------------------------------------------------------------------
-- Type-level booleans
-------------------------------------------------------------------------------

-- | Conditional expression at the type level.
type family If (b :: Bool) (t :: k) (e :: k) :: k where
    If True  t e = t
    If False t e = e

-- | Negation of type-level Booleans.
type family Not (a :: Bool) :: Bool where
    Not True  = False
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
                       (xs :: OptVector a n)
                           :: Constraint where
    AllSatisfy c End            = Valid
    AllSatisfy c (x :* _ :- xs) = ((c x), AllSatisfy c xs)

-- | Create a new constraint which is valid only if every pair of elements in
-- the given optimised vectors satisfy the given binary constraint.
-- Analogue of 'zipWith' for constraints and optimised vectors.
type family AllPairsSatisfy (c  :: a -> b -> Constraint)
                            (xs :: OptVector a n) (ys :: OptVector b n)
                                :: Constraint where
    AllPairsSatisfy c End            End            = Valid
    AllPairsSatisfy c (x :* _ :- xs) (y :* _ :- ys) = ((c x y), AllPairsSatisfy c xs ys)

-- | Create a new constraint which is valid only if every pair of elements in
-- the given vectors satisfy the given binary constraint.
-- Analogue of 'zipWith' for constraints and vectors.
type family AllPairsSatisfy' (c  :: a -> b -> Constraint)
                            (xs :: Vector a n) (ys :: Vector b n)
                                :: Constraint where
    AllPairsSatisfy' c None       None       = Valid
    AllPairsSatisfy' c (x :-- xs) (y :-- ys) = ((c x y), AllPairsSatisfy' c xs ys)

-- | Create a new constraint which is valid only if the given value satisfies
-- every unary constraint in the given list.
type family SatisfiesAll (cs :: [a -> Constraint])
                         (xs :: a)
                             :: Constraint where
    SatisfiesAll '[]      a = Valid
    SatisfiesAll (c : cs) a = (c a, SatisfiesAll cs a)

-- | Create a new constraint which is valid only if every element in the given
-- vector satisfies every unary constraint in the given list.
type family AllSatisfyAll (c1 :: [a -> Constraint])
                          (xs :: Vector a n)
                              :: Constraint where
    AllSatisfyAll _ None        = Valid
    AllSatisfyAll cs (v :-- vs) = (SatisfiesAll cs v, AllSatisfyAll cs vs)
