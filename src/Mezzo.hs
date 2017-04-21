
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo
-- Description :  Mezzo core module
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Module providing the external interface to the Mezzo library and EDSL.
--
-----------------------------------------------------------------------------

module Mezzo (module X, ($)) where

-- Uses import/export shortcut as suggested by HLint.

import Mezzo.Render as X
import Mezzo.Compose as X
import Mezzo.Model as X
    ( Music (..)
    , Voice (..)
    , Partiture (..)
    )
