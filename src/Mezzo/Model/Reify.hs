{-# LANGUAGE TypeInType, ScopedTypeVariables, TypeFamilies, FlexibleInstances,
    UndecidableInstances, ConstraintKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Model.Reify
-- Description :  MIDI exporting
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of types which can be reified at the term level.
--
-----------------------------------------------------------------------------

module Mezzo.Model.Reify where

import Data.Kind

-- | Class of types which can have a primitive representation at runtime.
class Primitive (a :: k) where
    -- | The type of the primitive representation.
    type Rep a
    -- | Convert a singleton of the type into its primitive representation.
    prim :: sing a -> Rep a
    -- | Pretty print a singleton of the type.
    pretty :: sing a -> String

instance {-# OVERLAPPABLE #-} Primitive t => Show (sing t) where
    show = pretty

-- | Primitive types with integer representations.
type IntRep t = (Primitive t, Rep t ~ Int)

-- | Primitive types with integer list representations.
type IntListRep t = (Primitive t, Rep t ~ [Int])

type FunRep a b t = (Primitive t, Rep t ~ (a -> b))
