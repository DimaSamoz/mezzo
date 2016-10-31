{-# LANGUAGE TypeInType, GADTs, TypeFamilies, ConstraintKinds #-}

module Lib
     where

import GHC.TypeLits
import GHC.Exts

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data PitchClass = C | D | E | F | G | A | B deriving (Eq, Show, Enum, Ord)

data Accidental = Nat | Flat | Sharp deriving (Eq, Show, Enum, Ord)

data Oct = Oct_1 | Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6 | Oct7 | Oct8 deriving (Eq, Show, Ord, Enum, Bounded)

data Duration = Whole | Half Duration deriving (Eq, Show)

data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave deriving (Eq, Show, Ord)

data Pitch (pc :: PitchClass) (a :: Accidental) (o :: Oct) where
    Pitch :: Pitch pc a o

data Note (p :: Pitch pc a o) (d :: Duration) where
    Note :: Ensure (ValidDuration d) => Note p d

type family ValidDuration (d :: Duration) :: Bool where
    ValidDuration Whole = True
    ValidDuration (Half (Half (Half (Half (Half Whole))))) = TypeError (Text "Note duration must be longer than 1/32.")
    ValidDuration (Half d) = ValidDuration d

-- type family ToInterval (pc1 :: PitchClass) (pc2 :: PitchClass) :: Interval where
--     ToInterval

type family Ensure (b :: Bool) :: Constraint where
    Ensure v = v ~ True

x :: Note p (Half (Half (Half (Half Whole))))
x = Note
