{-# LANGUAGE TypeInType, RankNTypes, GADTs, TypeOperators #-}

-- Opposite of the 'shouldNotTypecheck' function from
-- https://github.com/CRogers/should-not-typecheck
--

module TestUtils where

import Control.DeepSeq (force, NFData (..))
import Control.Exception (evaluate, try, TypeError (..))
import Test.HUnit.Lang (Assertion, assertFailure)

-- | Hetero-kinded propositional type equality.
-- If the type (a :~: b) is inhabited by Refl, a and b must be equal
data (a :: k1) :~: (b :: k2) where
  Refl :: a :~: a

-- | Instance which makes
instance NFData (a :~: b) where
    rnf Refl = ()

-- | Takes one argument, an expression that should  typecheck.
-- It will fail the test if the expression does not typecheck.
-- Requires Deferred Type Errors to be enabled for the file it is called in.
shouldTypecheck :: NFData a => (() ~ () => a) -> Assertion
shouldTypecheck a = do
  result <- try (evaluate $ force a)
  case result of
    Right _ -> return ()
    Left (TypeError msg) -> assertFailure $ "Expected expression to compile but it did not compile: \n" ++ msg
