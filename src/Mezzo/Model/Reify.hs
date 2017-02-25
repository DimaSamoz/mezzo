{-# LANGUAGE TypeInType, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}

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

-- | Class of types which can have a primitive representation at runtime.
class Primitive (a :: k) where
    prim :: proxy a -> Int
    pretty :: proxy a -> String

instance {-# OVERLAPPABLE #-} Primitive t => Show (proxy t) where
    show = pretty
