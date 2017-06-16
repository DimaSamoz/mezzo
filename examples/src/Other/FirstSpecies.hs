{-# LANGUAGE TypeInType, TypeFamilies, FlexibleInstances, UndecidableInstances,
    MultiParamTypeClasses, ConstraintKinds #-}

module Other.FirstSpecies where

import Mezzo
import Mezzo.Model.Prim
import Mezzo.Model.Types
import Mezzo.Model.Rules.Strict
import Mezzo.Model.Rules.RuleSet

import GHC.TypeLits

data FirstSpecies = FirstSpecies

instance RuleSet FirstSpecies where
    type MelConstraints FirstSpecies m1 m2 = MelConstraints Strict m1 m2
    type HarmConstraints FirstSpecies m1 m2 = HarmConstraints Strict m1 m2
    type HomConstraints FirstSpecies m1 m2 = HomConstraints Strict m1 m2
    type NoteConstraints FirstSpecies _ d = ValidDur d
    type RestConstraints FirstSpecies d = ValidDur d
    type ChordConstraints FirstSpecies _ _ = InvalidChord
    type ProgConstraints FirstSpecies _ _ = InvalidProg
    type TriplConstraints FirstSpecies _ _ _ _ = InvalidTripl

class ValidDur (d :: Duration)
instance ValidDur Whole
instance {-# OVERLAPPABLE #-}
            TypeError (Text "First species counterpoint can only have whole durations.")
            => ValidDur d

class InvalidChord
instance TypeError (Text "Chords are not allowed in first species counterpoint.")
         => InvalidChord

class InvalidProg
instance TypeError (Text "Progressions are not allowed in first species counterpoint.")
         => InvalidProg

class InvalidTripl
instance TypeError (Text "Triplets are not allowed in first species counterpoint.")
         => InvalidTripl

comp = score setRuleSet FirstSpecies withMusic (c maj qc :-: b qn)

main :: IO ()
main = renderScore "comp.mid" "Invalid composition" comp
