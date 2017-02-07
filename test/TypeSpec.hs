{-# OPTIONS_GHC -fdefer-type-errors #-}
-- {-# OPTIONS_GHC -w #-}
{-# LANGUAGE TypeInType, TypeOperators, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module TypeSpec where

import Test.Hspec
import GHC.TypeLits
import Control.Exception (evaluate)
import Data.Proxy
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Mezzo.Model.Types

typeSpec :: Spec
typeSpec =
    describe "Mezzo.Model.Types" $ do
        describe "Harmonic types" $ do
            it "should convert roots to pitches" $ do
                rootToPitch `shouldBe` True
                shouldNotTypecheck rootToPitchInv
            it "should sharpen roots" $ do
                sharpen `shouldBe` True
                shouldNotTypecheck sharpenInv
            it "should flatten roots" $ do
                flatten `shouldBe` True
                shouldNotTypecheck flattenInv
            it "should make intervals" $ do
                flatten `shouldBe` True

rootToPitch ::
        ( (RootToPitch (PitchRoot (Pitch C Natural Oct3)) ~ (Pitch C Natural Oct3))
        , (RootToPitch (DegreeRoot (Key D Flat MinorMode) IV) ~ (Pitch F Sharp Oct2))
        ) => Bool
rootToPitch = True

rootToPitchInv :: Proxy (RootToPitch (DegreeRoot (Key D Flat MinorMode) IV))
               -> Proxy (Pitch E Natural Oct2)
rootToPitchInv = id

sharpen :: ( (Sharpen (PitchRoot (Pitch C Natural Oct3))) ~ (PitchRoot (Pitch C Sharp Oct3))
           , (Sharpen (PitchRoot (Pitch E Natural Oct3))) ~ (PitchRoot (Pitch F Natural Oct3))
           , (Sharpen (PitchRoot (Pitch C Flat Oct3))) ~ (PitchRoot (Pitch C Natural Oct3))
           , (Sharpen (PitchRoot (Pitch B Natural Oct3))) ~ (PitchRoot (Pitch C Natural Oct4))
           , (Sharpen (DegreeRoot (Key F Sharp MinorMode) VI)) ~ (PitchRoot (Pitch D Sharp Oct3))
           , (Sharpen (DegreeRoot (Key E Sharp MajorMode) VII)) ~ (PitchRoot (Pitch F Natural Oct3))
           ) => Bool
sharpen = True

sharpenInv :: Proxy '(Sharpen (PitchRoot (Pitch B Flat Oct5)), Sharpen (DegreeRoot (Key C Flat MajorMode) IV))
           -> Proxy '(PitchRoot (Pitch C Flat Oct6), PitchRoot (Pitch F Natural Oct3))
sharpenInv = id

flatten :: ( (Flatten (PitchRoot (Pitch D Natural Oct3))) ~ (PitchRoot (Pitch D Flat Oct3))
           , (Flatten (PitchRoot (Pitch F Natural Oct3))) ~ (PitchRoot (Pitch E Natural Oct3))
           , (Flatten (PitchRoot (Pitch C Sharp Oct3))) ~ (PitchRoot (Pitch C Natural Oct3))
           , (Flatten (PitchRoot (Pitch C Natural Oct3))) ~ (PitchRoot (Pitch B Natural Oct2))
           , (Flatten (DegreeRoot (Key F Sharp MinorMode) VI)) ~ (PitchRoot (Pitch D Flat Oct3))
           , (Flatten (DegreeRoot (Key E Sharp MajorMode) VII)) ~ (PitchRoot (Pitch E Flat Oct3))
           ) => Bool
flatten = True

flattenInv :: Proxy '(Flatten (PitchRoot (Pitch C Sharp Oct5)), Flatten (DegreeRoot (Key C Flat MajorMode) V))
           -> Proxy '(PitchRoot (Pitch B Sharp Oct4), PitchRoot (Pitch E Natural Oct3))
flattenInv = id

makeInterval ::
            ( (MakeInterval (Pitch C Natural Oct3) (Pitch E Natural Oct3)) ~ (Interval Maj Third)
            , (MakeInterval (Pitch E Natural Oct3) (Pitch C Natural Oct3)) ~ (Interval Maj Third)
            , (MakeInterval (Pitch C Natural Oct3) (Pitch B Flat Oct3)) ~ (Interval Min Seventh)
            , (MakeInterval (Pitch E Sharp Oct3) (Pitch E Sharp Oct3)) ~ (Interval Perf Unison)
            , (MakeInterval (Pitch E Natural Oct3) (Pitch F Flat Oct3)) ~ (Interval Perf Unison)
            , (MakeInterval (Pitch E Sharp Oct3) (Pitch F Flat Oct3)) ~ (Interval Min Second)
            , (MakeInterval (Pitch F Sharp Oct3) (Pitch G Flat Oct3)) ~ (Interval Perf Unison)
            , (MakeInterval (Pitch C Natural Oct3) (Pitch C Natural Oct4)) ~ (Interval Perf Octave)
            , (MakeInterval (Pitch E Flat Oct3) (Pitch F Flat Oct4)) ~ Compound
            , (MakeInterval (Pitch E Sharp Oct3) (Pitch F Flat Oct4)) ~ (Interval Maj Seventh)
            ) => Bool
makeInterval = True
