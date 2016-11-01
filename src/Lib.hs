{-# LANGUAGE TypeInType, GADTs, TypeFamilies, ConstraintKinds, RankNTypes, TypeOperators #-}

module Lib
     where

import GHC.TypeLits
import GHC.Exts
import Data.Kind hiding (Constraint)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data PitchClass = C | D | E | F | G | A | B deriving (Eq, Show, Enum, Ord)

data Accidental = Nat | Flat | Sharp deriving (Eq, Show, Enum, Ord)

data Oct = Oct_1 | Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6 | Oct7 | Oct8 deriving (Eq, Show, Ord, Enum, Bounded)

-- data Duration = Duration :^: Duration | Whole | Half Duration deriving (Eq, Show)

type Whole = 32
type Half = 16
type Quarter = 8
type Eighth = 4
type Sixteenth = 2
type ThirtySecond = 1


data Duration :: Nat -> Type where
    Dur :: Duration n
    (:^:) :: Duration d1 -> Duration d2 -> Duration (d1 + d2)

data IntervalSize = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave deriving (Eq, Show, Ord)

data Pitch (pc :: PitchClass) (a :: Accidental) (o :: Oct) where
    Pitch :: Pitch pc a o

data Note :: Pitch pc a o -> Duration d -> Type where
    Note :: Note p d

-- type family ValidDuration (d :: Duration) :: Bool where
--     ValidDuration Whole = True
--     ValidDuration (Half (Half (Half (Half (Half Whole))))) = TypeError (Text "Note duration must be longer than 1/32.")
--     ValidDuration (Half d) = ValidDuration d
--
-- -- type family ToInterval (pc1 :: PitchClass) (pc2 :: PitchClass) :: Interval where
-- --     ToInterval
--
-- type family Ensure (b :: Bool) :: Constraint where
--     Ensure v = v ~ True
--
-- x :: Note p (Half (Half (Half (Half (Half Whole)))))
-- x = Note


-- data Music = Prim (forall p d . Note p d) | Music :-: Music | Music :|: Music

data Music :: Nat -> Type where
    Prim :: Note p (d :: Duration n) -> Music n
    (:-:) :: Music d1 -> Music d2 -> Music (d1 + d2)

-- type family (d1 :: Duration) +~ (d2 :: Duration)

-- type family SumLength (pl1 :: PieceLength) (pl2 :: PieceLength) :: PieceLength where
--     SumLength (PL n )
