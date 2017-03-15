
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose
-- Description :  Mezzo music description language
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Module providing the external interface to the Mezzo EDSL.
--
-----------------------------------------------------------------------------

module Mezzo.Compose (module X) where

-- Uses import/export shortcut as suggested by HLint.

import Mezzo.Compose.Basic as X
import Mezzo.Compose.Builder as X
import Mezzo.Compose.Types as X
import Mezzo.Compose.Combine as X
import Mezzo.Compose.Harmonic as X
