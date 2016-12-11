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
    , Head
    , Last
    , Matrix
    , type (++)
    , type (+*+)
    , type (+|+)
    , type (+-+)
    , Transpose
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
    ) where

import Data.Kind
import GHC.TypeLits

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

-- | A length-indexed vector.
data Vector :: Type -> Nat -> Type where
    Nil  :: Vector t 0
    (:-) :: t -> Vector t (n - 1) -> Vector t n

-- | Get the first element of the vector.
type family Head (v :: Vector t n) :: t where
    Head Nil      = TypeError (Text "Vector has no head element.")
    Head (v :- _) = v

-- | Get the last element of the vector.
type family Last (v :: Vector t n) :: t where
    Last Nil        = TypeError (Text "Vector has no last element.")
    Last (v :- Nil) = v
    Last (_ :- vs)  = Last vs

-- | A dimension-indexed matrix.
type Matrix t p q = Vector (Vector t q) p

-- | Type-level vector appending.
type family (x :: Vector t n) ++ (y :: Vector t m) :: Vector t (n + m) where
    Nil       ++ y  = y
    (x :- xs) ++ y = x :- (xs ++ y)

-- | Repeat the specified type n times.
type family (a :: t) +*+ (n :: Nat) :: Vector t n where
    x +*+ 0 = Nil
    x +*+ n = x :- (x +*+ (n - 1))

-- | Horizontal concatenation of type-level matrices.
-- Places the first matrix to the left of the second.
type family (a :: Matrix t p q) +|+ (b :: Matrix t p r) :: Matrix t p (q + r) where
    Nil         +|+ Nil         = Nil
    (r1 :- rs1) +|+ (r2 :- rs2) = (r1 ++ r2) :- (rs1 +|+ rs2)

-- | Vertical concatenation of type-level matrices.
-- Places the first matrix on top of the second.
type family (a :: Matrix t p r) +-+ (b :: Matrix t q r) :: Matrix t (p + q) r where
    m1 +-+ m2 = m1 ++ m2

-- | Transposition of type-level matrices.
type family Transpose (a :: Matrix t p q) :: Matrix t q p where
    Transpose Nil                = Nil
    Transpose (Nil :- Nil)       = Nil
    Transpose ((x :- xs) :- Nil) = (x :- Nil) :- (Transpose (xs :- Nil))
    Transpose (row :- rows)      = (Transpose (row :- Nil)) +|+ Transpose rows

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
type family AllSatisfy (c :: a -> Constraint) (xs :: Vector a n) :: Constraint where
    AllSatisfy c Nil = Valid
    AllSatisfy c (x :- xs) = ((c x), AllSatisfy c xs)
