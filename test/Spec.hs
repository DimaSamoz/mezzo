{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE TypeApplications, TypeInType, GADTs, BangPatterns #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq

import Mezzo.Model.Prim
import Mezzo.Model.Types
import Mezzo.Model.Music

main :: IO ()
main = hspec $ do
    describe "Mezzo.Model.Types.Music" $ do
        it "should not allow minor second harmonic intervals" $ do
            shouldNotTypecheck $
                -- (Note (PC @E) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
                -- (Note (PC @G) (Acc @Natural) (Oct @Oct3) (Dur @1))
                not 5

instance NFData (Music m) where
    rnf !(Note pc acc oct dur) = rnf pc `seq` rnf acc `seq` rnf oct `seq` rnf dur
    rnf !(Rest dur) = rnf dur
    rnf !(m1 :|: m2) = rnf m1 `seq` rnf m2
    rnf !(m1 :-: m2) = rnf m1 `seq` rnf m2

instance NFData (PC pc) where
    rnf !_ = ()

instance NFData (Acc acc) where
    rnf !_ = ()

instance NFData (Oct oct) where
    rnf !_ = ()

instance NFData (Dur dur) where
    rnf !_ = ()


--
-- bb =
--     ((Note (PC @G) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
--     (Note (PC @B) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
--     (Note (PC @D) (Acc @Natural) (Oct @Oct4) (Dur @1)) :-:
--     (Note (PC @F) (Acc @Sharp) (Oct @Oct4) (Dur @1)))
--     :|:
--     ((Note (PC @G) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
--     (Note (PC @C) (Acc @Natural) (Oct @Oct4) (Dur @1)) :-:
--     (Note (PC @E) (Acc @Natural) (Oct @Oct4) (Dur @1)) :-:
--     ((Rest (Dur @1))))
--
-- tch =
--     (((Note (PC @E) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
--     (Note (PC @G) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
--     (Note (PC @C) (Acc @Natural) (Oct @Oct4) (Dur @1)))
--         :|:
--     ((Note (PC @D) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
--     (Note (PC @F) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
--     (Note (PC @A) (Acc @Natural) (Oct @Oct3) (Dur @1))))
--     :-:
--     ((Note (PC @C) (Acc @Natural) (Oct @Oct2) (Dur @1))
--         :|:
--     (Note (PC @D) (Acc @Natural) (Oct @Oct2) (Dur @1)))

--
-- t =
--     ((Note (PC @E) (Acc @Natural) (Oct @Oct4) (Dur @1))
--         :|:
--     (Note (PC @D) (Acc @Natural) (Oct @Oct4) (Dur @1)))
--     :-:
--     ((Note (PC @C) (Acc @Natural) (Oct @Oct4) (Dur @1))
--         :|:
--     (Note (PC @G) (Acc @Natural) (Oct @Oct3) (Dur @1)))


chopR =
        ((Note (PC @E) (Acc @Natural) (Oct @Oct3) (Dur @2)) :-:
        (Note (PC @G) (Acc @Sharp) (Oct @Oct3) (Dur @2)) :-:
        (Note (PC @C) (Acc @Sharp) (Oct @Oct4) (Dur @2)))
        :|:
        ((Rest (Dur @1)) :-: (Rest (Dur @1)) :-: (Rest (Dur @1)))
        :|:
        ((Note (PC @C) (Acc @Sharp) (Oct @Oct3) (Dur @1)) :-:
        (Note (PC @G) (Acc @Sharp) (Oct @Oct3) (Dur @1)) :-:
        (Note (PC @B) (Acc @Natural) (Oct @Oct3) (Dur @1)))
        :|:
        ((Note (PC @C) (Acc @Sharp) (Oct @Oct3) (Dur @2)) :-:
        (Note (PC @F) (Acc @Sharp) (Oct @Oct3) (Dur @2)) :-:
        (Note (PC @A) (Acc @Natural) (Oct @Oct3) (Dur @2)))
        :|:
        ((Rest (Dur @1)) :-: (Rest (Dur @1)) :-: (Rest (Dur @1)))
        :|:
        ((Note (PC @C) (Acc @Sharp) (Oct @Oct3) (Dur @1)) :-:
        (Note (PC @E) (Acc @Natural) (Oct @Oct3) (Dur @1)) :-:
        (Note (PC @G) (Acc @Sharp) (Oct @Oct3) (Dur @1)))
        :|:
        ((Note (PC @C) (Acc @Sharp) (Oct @Oct3) (Dur @2)) :-:
        (Note (PC @E) (Acc @Natural) (Oct @Oct3) (Dur @2)) :-:
        (Note (PC @G) (Acc @Natural) (Oct @Oct3) (Dur @2)))
        :|:
        ((Note (PC @B) (Acc @Sharp) (Oct @Oct2) (Dur @2)) :-:
        (Note (PC @D) (Acc @Sharp) (Oct @Oct3) (Dur @2)) :-:
        (Note (PC @G) (Acc @Sharp) (Oct @Oct3) (Dur @2)))

chopL =
        (Note (PC @C) (Acc @Sharp) (Oct @Oct1) (Dur @2))
        :|:
        (Rest (Dur @1))
        :|:
        (Note (PC @C) (Acc @Sharp) (Oct @Oct2) (Dur @1))
        :|:
        (Note (PC @F) (Acc @Sharp) (Oct @Oct2) (Dur @2))
        :|:
        (Rest (Dur @1))
        :|:
        (Note (PC @C) (Acc @Sharp) (Oct @Oct2) (Dur @1))
        :|:
        (Note (PC @A) (Acc @Natural) (Oct @Oct1) (Dur @2))
        :|:
        (Note (PC @G) (Acc @Sharp) (Oct @Oct1) (Dur @2))

chop :: Score
chop = Score $ chopL :-: chopR
