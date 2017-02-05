{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE TypeInType, TypeOperators, GADTs, TemplateHaskell #-}

module PrimSpec where

import Test.Hspec
import GHC.TypeLits
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Mezzo.Model.Prim

primSpec :: Spec
primSpec =
    describe "Mezzo.Model.Prim" $ do
        it "should replicate elements correctly" $ do
            timesReplicate `shouldBe` True
        it "should get the head of an optimised vector correctly" $ do
            optVectorHead `shouldBe` True
        it "should get the head of a vector correctly" $ do
            vectorHead `shouldBe` True
        it "should get the last element of an optimised vector correctly" $ do
            optVectorLast `shouldBe` True
        it "should get the tail of a vector correctly" $ do
            vectorTail `shouldBe` True
        it "should get the initial elements of a vector correctly" $ do
            vectorInit `shouldBe` True
        it "should get the length of an optimised vector correctly" $ do
            optVectorLength `shouldBe` True
        it "should get the length of a vector correctly" $ do
            vectorLength `shouldBe` True

        it "should append optimised vectors correctly" $ do
            optVectorAppend `shouldBe` True
        it "should append vectors correctly" $ do
            vectorAppend `shouldBe` True


timesReplicate :: (True ** 23) ~ (True :* (T :: Times 23)) => Bool
timesReplicate = True

optVectorHead :: (Head (True ** 4 :- End)) ~ True => Bool
optVectorHead = True

vectorHead :: (Head' (True :-- None)) ~ True => Bool
vectorHead = True

optVectorLast :: (Last (True ** 4 :- False ** 9 :- End)) ~ False => Bool
optVectorLast = True

vectorTail :: (Tail' (True :-- False :-- None)) ~ (False :-- None) => Bool
vectorTail = True

vectorInit :: (Init' (True :-- False :-- None)) ~ (True :-- None) => Bool
vectorInit = True

optVectorLength :: (Length (True ** 4 :- False ** 9 :- End)) ~ 13 => Bool
optVectorLength = True

vectorLength :: (Length' (True :-- False :-- None)) ~ 2 => Bool
vectorLength = True

optVectorAppend :: ((True ** 5 :- False ** 2 :- End) ++ End ++ (False ** 9 :- End))
                  ~ (True ** 5 :- False ** 2 :- False ** 9 :- End) => Bool
optVectorAppend = True

vectorAppend :: ((2 :-- 43 :-- None) ++. None ++. (6 :-- None))
                  ~ (2 :-- 43 :-- 6 :-- None) => Bool
vectorAppend = True


{- MALFORMED TYPE SIGNATURES: should never typecheck
-- (Comment out line above to expose)

timesReplicateW :: (True ** 23) ~ (True :* (T :: Times 1)) => Bool
optVectorHeadW :: (Head (True ** 4 :- End)) ~ False => Bool
vectorHeadW :: (Head' (True :-- None)) ~ False => Bool
optVectorLastW :: (Last (True ** 4 :- False ** 9 :- End)) ~ True => Bool
vectorTailW :: (Tail' (True :-- False :-- None)) ~ (True :-- None) => Bool
vectorInitW :: (Init' (True :-- False :-- None)) ~ (False :-- None) => Bool
optVectorLengthW :: (Length (True ** 4 :- False ** 9 :- End)) ~ 12 => Bool
vectorLengthW :: (Length' (True :-- False :-- None)) ~ 3 => Bool
optVectorAppendW :: ((True ** 5 :- False ** 2 :- End) ++ (False ** 9 :- End))
                  ~ (True ** 5 :- False ** 11 :- End) => Bool
vectorAppend :: ((2 :-- 43 :-- None) ++. None ++. (6 :-- None))
                ~ (2 :-- 3 :-- 6 :-- None) => Bool

timesReplicateW = True
optVectorHeadW = True
vectorHeadW = True
optVectorLastW = True
vectorTailW = True
vectorInitW = True
optVectorLengthW = True
vectorLengthW = True
optVectorAppendW = True
vectorAppend = True


-- -}
