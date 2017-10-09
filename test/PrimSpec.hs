{-# OPTIONS_GHC -fdefer-type-errors  #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE TypeInType, TypeOperators, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module PrimSpec where

import Test.Hspec
import GHC.TypeLits
import Control.Exception (evaluate)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import TestUtils

import Mezzo.Model.Prim


primSpec :: Spec
primSpec =
    describe "Mezzo.Model.Prim" $ do
        describe "Vector operations" $ do
            it "should replicate elements" $ do
                shouldTypecheck timesReplicate
                shouldNotTypecheck timesReplicate'
                shouldNotTypecheck timesReplicate''

            it "should get the head of an optimised vector" $ do
                shouldTypecheck optVectorHead
                shouldNotTypecheck optVectorHead'

            it "should get the head of a vector" $ do
                shouldTypecheck vectorHead
                shouldNotTypecheck vectorHead'

            it "should get the last element of an optimised vector" $ do
                shouldTypecheck optVectorLast
                shouldNotTypecheck optVectorLast'

            it "should get the tail of a vector" $ do
                shouldTypecheck vectorTail
                shouldNotTypecheck vectorTail'
                shouldNotTypecheck vectorTail''

            it "should get the initial elements of a vector" $ do
                shouldTypecheck vectorInit
                shouldNotTypecheck vectorInit'
                shouldNotTypecheck vectorInit'

            it "should get the length of an optimised vector" $ do
                shouldTypecheck optVectorLength
                shouldNotTypecheck optVectorLength'

            it "should get the length of a vector" $ do
                shouldTypecheck vectorLength
                shouldNotTypecheck vectorLength'

            it "should append optimised vectors" $ do
                shouldTypecheck optVectorAppend
                shouldNotTypecheck optVectorAppend'
                shouldNotTypecheck optVectorAppend''

            it "should append vectors" $ do
                shouldTypecheck vectorAppend
                shouldNotTypecheck vectorAppend'
                shouldNotTypecheck vectorAppend''

            it "should add an element to the end of a vector" $ do
                shouldTypecheck snoc
                shouldNotTypecheck snoc'
                shouldNotTypecheck snoc''

            it "should repeat an element to an optimised vector" $ do
                shouldTypecheck repeatVec
                shouldNotTypecheck repeatVec'
                shouldNotTypecheck repeatVec''

        describe "Matrix operations" $ do
            it "should horizontally concatenate matrices" $ do
                shouldTypecheck matHConcatNorm
                shouldNotTypecheck matHConcatNorm'
                shouldNotTypecheck matHConcatNorm''
                shouldTypecheck matHConcatRE
                shouldNotTypecheck matHConcatRE'
                shouldTypecheck matHConcatLE
                shouldNotTypecheck matHConcatLE'
                -- shouldTypecheck matHConcatBE

            it "should vertically concatenate matrices" $ do
                shouldTypecheck matVConcat
            it "should vertically align matrices" $ do
                shouldTypecheck align
            it "should convert vectors to column matrices" $ do
                shouldTypecheck vecToColMatrix

        describe "Arithmetic operations" $ do
            it "should calculate maximum of two numbers" $ do
                shouldTypecheck maxNat
            it "should calculate minimum of two numbers" $ do
                shouldTypecheck minNat

        describe "Constraint operations" $ do
            it "should apply a constraint to an optimised vector" $ do
                shouldTypecheck allSatisfy
                -- shouldNotTypecheck allSatisfyInv
            it "should apply a binary constraint to two optimised vectors" $ do
                shouldTypecheck allPairsSatisfy
                shouldNotTypecheck allPairsSatisfyInv
            it "should apply a binary constraint to two vectors" $ do
                shouldTypecheck allPairsSatisfy'
                shouldNotTypecheck allPairsSatisfyInv'
            it "should apply all constraints to a value" $ do
                shouldTypecheck satisfiesAll
                -- shouldNotTypecheck satisfiesAllInv
            it "should apply all constraints to all values" $ do
                shouldTypecheck allSatisfyAll
                -- shouldNotTypecheck allSatisfyAllInv

---------------------
-- Vector operations
---------------------

timesReplicate :: (True ** 23) :~: (True :* (T :: Times 23))
timesReplicate = Refl
timesReplicate' :: (True ** 23) :~: (False :* (T :: Times 23))
timesReplicate' = Refl
timesReplicate'' :: (True ** 23) :~: (True :* (T :: Times 13))
timesReplicate'' = Refl

optVectorHead :: (Head (True ** 4 :- End)) :~: True
optVectorHead = Refl
optVectorHead' :: (Head (True ** 4 :- End)) :~: False
optVectorHead' = Refl

vectorHead :: (Head' (True :-- None)) :~: True
vectorHead = Refl
vectorHead' :: (Head' (True :-- None)) :~: False
vectorHead' = Refl

optVectorLast :: (Last (True ** 4 :- False ** 9 :- End)) :~: False
optVectorLast = Refl
optVectorLast' :: (Last (True ** 4 :- False ** 9 :- End)) :~: True
optVectorLast' = Refl

vectorTail :: (Tail' (True :-- False  :-- True :-- None)) :~: (False :-- True :-- None)
vectorTail = Refl
vectorTail' :: (Tail' (True :-- False  :-- True :-- None)) :~: (False :-- False :-- None)
vectorTail' = Refl
vectorTail'' :: (Tail' (True :-- False  :-- True :-- None)) :~: (False :-- None)
vectorTail'' = Refl

vectorInit :: (Init' (True :-- False  :-- True :-- None)) :~: (True :-- False :-- None)
vectorInit = Refl
vectorInit' :: (Init' (True :-- False  :-- True :-- None)) :~: (False :-- False :-- None)
vectorInit' = Refl
vectorInit'' :: (Init' (True :-- False  :-- True :-- None)) :~: (False :-- None)
vectorInit'' = Refl

optVectorLength :: (Length (True ** 4 :- False ** 9 :- End)) :~: 13
optVectorLength = Refl
optVectorLength' :: (Length (True ** 4 :- False ** 9 :- End)) :~: 6
optVectorLength' = Refl

vectorLength :: (Length' (True :-- False :-- None)) :~: 2
vectorLength = Refl
vectorLength' :: (Length' (True :-- False :-- None)) :~: 5
vectorLength' = Refl

optVectorAppend :: ((True ** 5 :- False ** 2 :- End) ++ End ++ (False ** 9 :- End))
                :~: (True ** 5 :- False ** 2 :- False ** 9 :- End)
optVectorAppend = Refl
optVectorAppend' :: ((True ** 5 :- False ** 2 :- End) ++ End ++ (False ** 9 :- End))
                 :~: (True ** 5 :- True ** 2 :- False ** 9 :- End)
optVectorAppend' = Refl
optVectorAppend'' :: ((True ** 5 :- False ** 2 :- End) ++ End ++ (False ** 9 :- End))
                  :~: (True ** 5 :- False ** 1 :- False ** 9 :- End)
optVectorAppend'' = Refl
optVectorAppend''' :: ((True ** 5 :- False ** 2 :- End) ++ End ++ (False ** 9 :- End))
                   :~: (True ** 5 :- False ** 11 :- End)
optVectorAppend''' = Refl

vectorAppend :: ((2 :-- 43 :-- None) ++. None ++. (6 :-- None))
             :~: (2 :-- 43 :-- 6 :-- None)
vectorAppend = Refl
vectorAppend' :: ((2 :-- 43 :-- None) ++. None ++. (6 :-- None))
              :~: (2 :-- 43 :-- 1 :-- None)
vectorAppend' = Refl
vectorAppend'' :: ((2 :-- 43 :-- None) ++. None ++. (6 :-- None))
               :~: (2 :-- 1 :-- None)
vectorAppend'' = Refl

snoc :: ((2 :-- 5 :-- None) :-| 6) :~: (2 :-- 5 :-- 6 :-- None)
snoc = Refl
snoc' :: ((2 :-- 5 :-- None) :-| 2) :~: (2 :-- 5 :-- 6 :-- None)
snoc' = Refl
snoc'' :: ((2 :-- 5 :-- None) :-| 2) :~: (2 :-- 4 :-- 5 :-- 6 :-- None)
snoc'' = Refl

repeatVec :: (True +*+ 3) :~: (True ** 3 :- End)
repeatVec = Refl
repeatVec' :: (True +*+ 3) :~: (False ** 3 :- End)
repeatVec' = Refl
repeatVec'' :: (True +*+ 3) :~: (True ** 2 :- End)
repeatVec'' = Refl

---------------------
-- Matrix operations
---------------------
-- Harmonic concatenation

matHConcatNorm :: ((True ** 2 :- End :-- False ** 2 :- End :-- None)
              +|+ (False ** 5 :- End :-- True ** 5 :- End :-- None))
            :~: ((True ** 2 :- False ** 5 :- End :-- False ** 2 :- True ** 5 :- End :-- None))
matHConcatNorm = Refl
matHConcatNorm' :: ((True ** 2 :- End :-- False ** 2 :- End :-- None)
               +|+ (False ** 5 :- End :-- False ** 5 :- End :-- None))
            :~: ((True ** 2 :- False ** 5 :- End :-- False ** 2 :- True ** 5 :- End :-- None))
matHConcatNorm' = Refl
matHConcatNorm'' :: ((True ** 2 :- End :-- False ** 2 :- End :-- None)
              +|+ (False ** 5 :- End :-- True ** 5 :- End :-- None))
            :~: ((True ** 5 :- False ** 2 :- End :-- False ** 2 :- True ** 5 :- End :-- None))
matHConcatNorm'' = Refl

matHConcatRE :: ((True ** 2 :- End :-- False ** 2 :- End :-- None)
             +|+ (End :-- End :-- None))
            :~: (True ** 2 :- End :-- False ** 2 :- End :-- None)
matHConcatRE = Refl
matHConcatRE' :: ((True ** 2 :- End :-- False ** 2 :- End :-- None)
              +|+ (End :-- End :-- None))
            :~: (True ** 2 :- End :-- True ** 2 :- End :-- None)
matHConcatRE' = Refl

matHConcatLE :: ((True ** 2 :- End :-- False ** 2 :- End :-- None)
             +|+ (End :-- End :-- None))
            :~: (True ** 2 :- End :-- False ** 2 :- End :-- None)
matHConcatLE = Refl
matHConcatLE' :: ((True ** 2 :- End :-- False ** 2 :- End :-- None)
              +|+ (End :-- End :-- None))
            :~: (True ** 2 :- End :-- True ** 2 :- End :-- None)
matHConcatLE' = Refl

matHConcatBE :: ((None :: Matrix t 0 0) +|+ None) :~: None
matHConcatBE = Refl

-- Vertical concatenation
matVConcatNF :: ((True ** 3 :- False ** 6 :- End :-- None)
             +-+ (False ** 3 :- True ** 6 :- End :-- None))
        :~: (True ** 3 :- False ** 6 :- End :-- False ** 3 :- True ** 6 :- End :-- None)
matVConcatNF = Refl
matVConcatNF' :: ((True ** 3 :- False ** 6 :- End :-- None)
              +-+ (False ** 3 :- True ** 6 :- End :-- None))
        :~: (True ** 3 :- False ** 6 :- End :-- True ** 3 :- True ** 6 :- End :-- None)
matVConcatNF' = Refl
matVConcatNF'' :: ((True ** 3 :- False ** 6 :- End :-- None)
             +-+ (False ** 3 :- True ** 6 :- End :-- None))
        :~: (True ** 2 :- False ** 7 :- End :-- False ** 3 :- True ** 6 :- End :-- None)
matVConcatNF'' = Refl

matVConcatTF :: ((True ** 9 :- End :-- None)
             +-+ (False ** 3 :- True ** 6 :- End :-- None))
        :~: (True ** 3 :- True ** 6 :- End :-- False ** 3 :- True ** 6 :- End :-- None)
matVConcatTF = Refl
matVConcatTF' :: ((True ** 9 :- End :-- None)
             +-+ (False ** 3 :- True ** 6 :- End :-- None))
        :~: (True ** 3 :- True ** 6 :- End :-- True ** 3 :- True ** 6 :- End :-- None)
matVConcatTF' = Refl
matVConcatTF'' :: ((True ** 9 :- End :-- None)
             +-+ (False ** 3 :- True ** 6 :- End :-- None))
        :~: (True ** 3 :- True ** 6 :- End :-- False ** 2 :- True ** 7 :- End :-- None)
matVConcatTF'' = Refl


align ::
          -- Right fragment
        ( Align (True ** 2 :- False ** 2 :- End :-- None) (True ** 4 :- End :-- None)
          ~ '(True ** 2 :- False ** 2 :- End :-- None, True ** 2 :- True ** 2 :- End :-- None)
          -- Left fragment
        , Align (False ** 18 :- End :-- None) (True ** 8 :- False ** 10 :- End :-- None)
          ~ '(False ** 8 :- False ** 10 :- End :-- None, True ** 8 :- False ** 10 :- End :-- None)
          -- No fragment
        , Align (True ** 4 :- End :-- None) (False ** 4 :- End :-- None)
          ~ '(True ** 4 :- End :-- None, False ** 4 :- End :-- None)
          -- Both fragment
        , Align (False ** 18 :- True ** 3 :- End :-- None) (True ** 8 :- False ** 13 :- End :-- None)
          ~ '(False ** 8 :- False ** 10 :- True ** 3 :- End :-- None, True ** 8 :- False ** 10 :- False ** 3 :- End :-- None)
        ) => Bool
align = True

matVConcat ::
          -- No fragmentation
        (    ((True ** 3 :- False ** 6 :- End :-- None)
          +-+ (False ** 3 :- True ** 6 :- End :-- None))
          ~ (True ** 3 :- False ** 6 :- End :-- False ** 3 :- True ** 6 :- End :-- None)
          -- Top fragment
        ,    ((True ** 9 :- End :-- None)
          +-+ (False ** 3 :- True ** 6 :- End :-- None))
          ~ (True ** 3 :- True ** 6 :- End :-- False ** 3 :- True ** 6 :- End :-- None)
          -- Bottom fragment
        ,    ((True ** 3 :- False ** 6 :- End :-- None)
          +-+ (False ** 9 :- End :-- None))
          ~ (True ** 3 :- False ** 6 :- End :-- False ** 3 :- False ** 6 :- End :-- None)
          -- Both fragment
        ,    ((True ** 3 :- False ** 6 :- End :-- None)
          +-+ (False ** 6 :- True ** 3 :- End :-- None))
          ~ (True ** 3 :- False ** 3 :- False ** 3 :- End :-- False ** 3 :- False ** 3 :- True ** 3 :- End :-- None)
          -- More voices
        ,    ((True ** 2 :- False ** 8 :- End :-- False ** 2 :- True ** 8 :- End :-- None)
          +-+ (False ** 4 :- True ** 6 :- End :-- None)
          +-+ (True ** 1 :- False ** 8 :- True ** 1 :- End :-- False ** 1 :- True ** 8 :- False ** 1 :- End :-- None))
          ~    ((True ** 1 :- True ** 1 :- False ** 2 :- False ** 5 :- False ** 1 :- End)
            :-- (False ** 1 :- False ** 1 :- True ** 2 :- True ** 5 :- True ** 1 :- End)
            :-- (False ** 1 :- False ** 1 :- False ** 2 :- True ** 5 :- True ** 1 :- End)
            :-- (True ** 1 :- False ** 1 :- False ** 2 :- False ** 5 :- True ** 1 :- End)
            :-- (False ** 1 :- True ** 1 :- True ** 2 :- True ** 5 :- False ** 1 :- End) :-- None
            )
        ) => Bool
matVConcat = True

vecToColMatrix ::
        ( (VectorToColMatrix 2 (True :-- False :-- None) 24)
          ~ (False ** 24 :- End :-- True ** 24 :- End :-- None)
        , (VectorToColMatrix 0 None 11) ~ None
        ) => Bool
vecToColMatrix = True


-- Let's assume that the Boolean functions work...

maxNat :: ((MaxN 5 9) ~ 9, (MaxN 7 4) ~ 7, (MaxN 0 8) ~ 8, (MaxN 1 0) ~ 1, (MaxN 3 3) ~ 3
          , (MaxN (MaxN 133 43) 235) ~ 235) => Bool
maxNat = True

minNat :: ((MinN 5 9) ~ 5, (MinN 7 4) ~ 4, (MinN 0 8) ~ 0, (MinN 1 0) ~ 0, (MinN 3 3) ~ 3
          , (MinN (MinN 133 43) 235) ~ 43) => Bool
minNat = True

allSatisfy :: (AllSatisfy Num (Int ** 1 :- Integer ** 1 :- Double ** 1 :- End)) => Bool
allSatisfy = True

-- allSatisfyInv :: (AllSatisfy Integral (Double ** 1 :- String ** 1 :- Bool ** 1 :- End)) => Bool
-- allSatisfyInv = True

class TestConstraint (a :: Nat) (b :: Nat)
instance (MaxN a b ~ b) => TestConstraint a b

allPairsSatisfy :: (AllPairsSatisfy TestConstraint
                    (1 ** 1 :- 3 ** 1 :- 5 ** 1 :- End) (2 ** 1 :- 4 ** 1 :- 6 ** 1 :- End)) => Bool
allPairsSatisfy = True

allPairsSatisfyInv :: (AllPairsSatisfy TestConstraint (4 ** 1 :- End) (1 ** 1 :- End)) => Bool
allPairsSatisfyInv = True

allPairsSatisfy' :: (AllPairsSatisfy' TestConstraint
                    (1 :-- 3 :-- 5 :-- None) (2 :-- 4 :-- 6 :-- None)) => Bool
allPairsSatisfy' = True

allPairsSatisfyInv' :: (AllPairsSatisfy' TestConstraint (2 :-- None) (1 :-- None)) => Bool
allPairsSatisfyInv' = True

satisfiesAll :: (SatisfiesAll '[Num, Integral, Show] Int) => Bool
satisfiesAll = True

-- Doesn't work unfortunately
-- satisfiesAllInv :: (SatisfiesAll '[Num, Integral, Show] Bool) => Bool
-- satisfiesAllInv = True

allSatisfyAll :: (AllSatisfyAll '[Num, Show] (Double :-- Int :-- Integer :-- None)) => Bool
allSatisfyAll = True

-- allSatisfyAllInv :: (AllSatisfyAll '[Num, Show] (Bool :-- String :-- Integer :-- None)) => Bool
-- allSatisfyAllInv = True



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
vectorAppendW :: ((2 :-- 43 :-- None) ++. None ++. (6 :-- None))
                ~ (2 :-- 3 :-- 6 :-- None) => Bool
snocW :: ((2 :-- 5 :-- None) :-| 6) ~ (2 :-- 5 :-- 4 :-- None) => Bool
repeatVecW :: (True +*+ 3) ~ (True ** 4 :- End) => Bool
matHConcatW ::
        (  ((True ** 2 :- End :-- False ** 2 :- End :-- None)
           +|+ (False ** 5 :- End :-- True ** 5 :- End :-- None))
           ~ ((True ** 2 :- False ** 5 :- End :-- True ** 2 :- True ** 5 :- End :-- None))

        ,  ((True ** 2 :- End :-- False ** 2 :- End :-- None)
           +|+ (End :-- None))
           ~ (True ** 2 :- End :-- False ** 2 :- End :-- None)

        , (None +|+ None) ~ (None :-- None)
        )
          => Bool
alignW ::
          -- Right fragment
        ( Align (True ** 2 :- False ** 2 :- End :-- None) (True ** 4 :- End :-- None)
          ~ '(True ** 2 :- False ** 2 :- End :-- None, True ** 1 :- True ** 3 :- End :-- None)
          -- Left fragment
        , Align (False ** 18 :- End :-- None) (True ** 8 :- False ** 10 :- End :-- None)
          ~ '(False ** 7 :- False ** 11 :- End :-- None, True ** 8 :- False ** 10 :- End :-- None)
          -- No fragment
        , Align (True ** 4 :- End :-- None) (False ** 4 :- End :-- None)
          ~ '(True ** 4 :- End :-- None, True ** 4 :- End :-- None)
          -- Both fragment
        , Align (False ** 18 :- True ** 3 :- End :-- None) (True ** 8 :- False ** 13 :- End :-- None)
          ~ '(False ** 8 :- False ** 9 :- True ** 4 :- End :-- None, True ** 8 :- False ** 10 :- False ** 3 :- End :-- None)
        ) => Bool
vecToColMatrixW ::
        ( (VectorToColMatrix (True :-- False :-- None) 24)
          ~ (False ** 24 :- End :-- False ** 24 :- End :-- None)
        , (VectorToColMatrix None 11) ~ (True ** 11 :- End :-- None)
        ) => Bool

allSatisfyW :: (AllSatisfy Num (Int ** 1 :- Bool ** 1 :- Double ** 1 :- End)) => Bool


timesReplicateW = True
optVectorHeadW = True
vectorHeadW = True
optVectorLastW = True
vectorTailW = True
vectorInitW = True
optVectorLengthW = True
vectorLengthW = True
optVectorAppendW = True
vectorAppendW = True
snocW = True
repeatVecW = True
matHConcatW = True
alignW = True
vecToColMatrixW = True
allSatisfyW = True

-- -}
