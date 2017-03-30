{-# LANGUAGE CPP, RankNTypes, GADTs #-}

-- Opposite of the 'shouldNotTypecheck' function from
-- https://github.com/CRogers/should-not-typecheck
--

module ShouldTypecheck (shouldTypecheck) where

import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError (..))
import Test.HUnit.Lang (Assertion, assertFailure)

{-|
  Takes one argument, an expression that should  typecheck.
  It will fail the test if the expression does not typecheck.
  Requires Deferred Type Errors to be enabled for the file it is called in.

-}
shouldTypecheck :: NFData a => (() ~ () => a) -> Assertion
shouldTypecheck a = do
  result <- try (evaluate $ force a)
  case result of
    Right _ -> return ()
    Left e@(TypeError msg) -> assertFailure $ "Expected expression to compile but it did not compile: \n" ++ msg
