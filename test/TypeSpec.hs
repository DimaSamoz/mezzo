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
import Mezzo.Model.Prim
import Mezzo.Model.Harmony

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
                makeInterval `shouldBe` True
            it "should shift pitches by an interval" $
                shift `shouldBe` True
        describe "Chords" $ do
            it "should convert chords to Music" $
                fromChord `shouldBe` True

rootToPitch ::
        ( (RootToPitch (PitchRoot (Pitch C Natural Oct3)) ~ (Pitch C Natural Oct3))
        , (RootToPitch (DegreeRoot (Key D Flat MinorMode) (Degree IV Natural Oct2)) ~ (Pitch F Sharp Oct2))
        ) => Bool
rootToPitch = True

rootToPitchInv :: Proxy (RootToPitch (DegreeRoot (Key D Flat MinorMode) (Degree IV Natural Oct2)))
               -> Proxy (Pitch E Natural Oct2)
rootToPitchInv = id

sharpen :: ( (Sharpen (PitchRoot (Pitch C Natural Oct3))) ~ (PitchRoot (Pitch C Sharp Oct3))
           , (Sharpen (PitchRoot (Pitch E Natural Oct3))) ~ (PitchRoot (Pitch F Natural Oct3))
           , (Sharpen (PitchRoot (Pitch C Flat Oct3))) ~ (PitchRoot (Pitch C Natural Oct3))
           , (Sharpen (PitchRoot (Pitch B Natural Oct3))) ~ (PitchRoot (Pitch C Natural Oct4))
           , (Sharpen (DegreeRoot (Key F Sharp MinorMode) (Degree VI Natural Oct3))) ~ (PitchRoot (Pitch D Sharp Oct4))
           , (Sharpen (DegreeRoot (Key E Sharp MajorMode) (Degree VII Natural Oct3))) ~ (PitchRoot (Pitch F Natural Oct4))
           ) => Bool
sharpen = True

sharpenInv :: Proxy '(Sharpen (PitchRoot (Pitch B Flat Oct5)), Sharpen (DegreeRoot (Key C Flat MajorMode) (Degree IV Natural Oct3)))
           -> Proxy '(PitchRoot (Pitch C Flat Oct6), PitchRoot (Pitch F Natural Oct3))
sharpenInv = id

flatten :: ( (Flatten (PitchRoot (Pitch D Natural Oct3))) ~ (PitchRoot (Pitch D Flat Oct3))
           , (Flatten (PitchRoot (Pitch F Natural Oct3))) ~ (PitchRoot (Pitch E Natural Oct3))
           , (Flatten (PitchRoot (Pitch C Sharp Oct3))) ~ (PitchRoot (Pitch C Natural Oct3))
           , (Flatten (PitchRoot (Pitch C Natural Oct3))) ~ (PitchRoot (Pitch B Natural Oct2))
           , (Flatten (DegreeRoot (Key F Sharp MinorMode) (Degree VI Natural Oct3))) ~ (PitchRoot (Pitch D Flat Oct4))
           , (Flatten (DegreeRoot (Key E Sharp MajorMode) (Degree VII Natural Oct3))) ~ (PitchRoot (Pitch E Flat Oct4))
           ) => Bool
flatten = True

flattenInv :: Proxy '(Flatten (PitchRoot (Pitch C Sharp Oct5)), Flatten (DegreeRoot (Key C Flat MajorMode) (Degree V Natural Oct3)))
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
            , (MakeInterval (Pitch C Natural Oct3) (Pitch F Sharp Oct3)) ~ (Interval Aug Fourth)
            , (MakeInterval (Pitch C Natural Oct3) (Pitch G Flat Oct3)) ~ (Interval Dim Fifth)
            ) => Bool
makeInterval = True

shift ::
            ( (RaiseBy (Pitch C Natural Oct3) (Interval Maj Third) ~ (Pitch E Natural Oct3))
            , (LowerBy (Pitch E Natural Oct3) (Interval Maj Third)) ~ (Pitch C Natural Oct3)
            , (RaiseBy (Pitch C Natural Oct3) (Interval Min Seventh)) ~ (Pitch B Flat Oct3)
            , (RaiseBy (Pitch E Sharp Oct3) (Interval Perf Unison)) ~ (Pitch E Sharp Oct3)
            , (RaiseBy (Pitch E Natural Oct3) (Interval Min Second)) ~ (Pitch F Natural Oct3)
            , (LowerBy (Pitch F Natural Oct3) (Interval Min Second)) ~ (Pitch E Natural Oct3)
            , (RaiseBy (Pitch C Natural Oct3) (Interval Perf Octave)) ~ (Pitch C Natural Oct4)
            , (LowerBy (Pitch C Natural Oct4) (Interval Perf Octave)) ~ (Pitch C Natural Oct3)
            , (RaiseBy (Pitch E Sharp Oct3) (Interval Maj Seventh)) ~ (Pitch E Natural Oct4)
            , (RaiseBy (Pitch C Natural Oct3) (Interval Aug Fourth)) ~ (Pitch F Sharp Oct3)
            , (LowerBy (Pitch F Natural Oct4) (Interval Aug Fourth)) ~ (Pitch B Natural Oct3)
            , (RaiseBy (Pitch C Natural Oct3) (Interval Dim Fifth)) ~ (Pitch G Flat Oct3)
            , (LowerBy (Pitch F Natural Oct4) (Interval Dim Fifth)) ~ (Pitch B Natural Oct3)
            ) => Bool
shift = True


fromChord ::
            ( (FromChord (Triad (PitchRoot (Pitch C Natural Oct4)) MajTriad Inv0) 8)
                ~ ( Pitch G Natural Oct4 ** 8 :- End
                :-- Pitch E Natural Oct4 ** 8 :- End
                :-- Pitch C Natural Oct4 ** 8 :- End :-- None)
            , (FromChord (Triad (PitchRoot (Pitch A Flat Oct4)) DimTriad Inv2) 8)
                ~ ( Pitch B Natural Oct5 ** 8 :- End
                :-- Pitch G Sharp Oct5 ** 8 :- End
                :-- Pitch D Natural Oct5 ** 8 :- End :-- None)
            , (FromChord (Tetrad (PitchRoot (Pitch G Natural Oct4)) MajMinSeventh Inv0) 8)
                ~ ( Pitch F Natural Oct5 ** 8 :- End
                :-- Pitch D Natural Oct5 ** 8 :- End
                :-- Pitch B Natural Oct4 ** 8 :- End
                :-- Pitch G Natural Oct4 ** 8 :- End :-- None)
            , (FromChord (Tetrad (PitchRoot (Pitch B Sharp Oct3)) HalfDimSeventh Inv3) 8)
                ~ ( Pitch F Sharp Oct5 ** 8 :- End
                :-- Pitch D Sharp Oct5 ** 8 :- End
                :-- Pitch C Natural Oct5 ** 8 :- End
                :-- Pitch B Flat Oct4 ** 8 :- End :-- None)
            , (FromChord (Tetrad (PitchRoot (Pitch C Natural Oct4)) (DoubledT MajTriad) Inv0) 8)
                ~ ( Pitch C Natural Oct5 ** 8 :- End
                :-- Pitch G Natural Oct4 ** 8 :- End
                :-- Pitch E Natural Oct4 ** 8 :- End
                :-- Pitch C Natural Oct4 ** 8 :- End :-- None)
            , (FromChord (Tetrad (PitchRoot (Pitch C Sharp Oct4)) (DoubledT AugTriad) Inv1) 8)
                ~ ( Pitch F Natural Oct5 ** 8 :- End
                :-- Pitch C Sharp Oct5 ** 8 :- End
                :-- Pitch A Natural Oct4 ** 8 :- End
                :-- Pitch F Natural Oct4 ** 8 :- End :-- None)
            ) => Bool
fromChord = True
