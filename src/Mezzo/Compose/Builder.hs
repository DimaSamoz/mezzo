{-# LANGUAGE TypeInType, RankNTypes, ExistentialQuantification, ImplicitParams #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Compose.Builder
-- Description :  Music builder
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Pattern of combinatorially building musical terms of various types.
--
-----------------------------------------------------------------------------

module Mezzo.Compose.Builder
    (
    -- * General builder types
      Spec
    , Conv
    , Mut
    , Mut'
    , Term
    -- * General builder types
    , RootS
    , ChorS
    , RootM
    , ChorM
    , ChorC
    , NoteT
    , ChorT
    )
    where

import Mezzo.Model

-------------------------------------------------------------------------------
-- General types
-------------------------------------------------------------------------------

-- | Specifier: specifies a value of some type which starts the building.
type Spec t = forall m. (t -> m) -> m

-- | Converter: converts a value of type s to a value of type t.
type Conv s t = s -> Spec t

-- | Mutator: mutates a value of type t.
type Mut t = Conv t t

-- | Terminator: finishes building a value of type t and returns a result of type r.
type Term t r = t -> r

-- | Flexible mutator: mutator that allows slight changes in the type (otherwise use 'Conv').
type Mut' t t' = Conv t t'

-- | Returns a new specifier for the given value.
spec :: t -> Spec t
spec i c = c i

-- | A mutator that does nothing.
nop :: Mut t
nop = spec

-------------------------------------------------------------------------------
-- Music specific types
-------------------------------------------------------------------------------

-- | Root specifier.
type RootS r = Spec (Root r)

-- | Chord specifier.
type ChorS c = Spec (Cho c)

-- | Root mutator.
type RootM r r' = Mut' (Root r) (Root r')

-- | Chord mutator.
type ChorM c c' = Mut' (Cho c) (Cho c')

-- | Converter from roots to chords.
type ChorC c r t i = Conv (Root r) (Cho (c r t i))

-- | Note terminator.
type NoteT r d = Term (Root r) (Music (FromRoot r d))

-- | Chord terminator.
type ChorT c d = Term (Cho c) (Music (FromChord c d))

inKey :: KeyS key -> a -> a
inKey key cont = let ?k = key in cont


i :: (?k :: KeyS key) => RootS (DegreeRoot key I)
i = spec Root

c :: RootS (PitchRoot (Pitch C Natural Oct3))
c = spec Root

sharp :: RootM r (Sharpen r)
sharp r = spec Root

maj :: ChorC Triad r MajTriad Inv0
maj r = spec Cho

qn :: NoteT r 8
qn p = Note p Dur

qc :: ChorT c 8
qc c = Chord c Dur


-------------------------------------------------------------------------------
-- Silly examples
-- Most of these are quite pointless, but show that the pattern enables us to
-- write DSLs with a very fluent feel.
-------------------------------------------------------------------------------

inc :: Mut Int
inc i = spec (succ i)

toString :: Conv Int String
toString n = spec (show n)

ex :: Mut String
ex s = spec (s ++ "!")

smile :: Term String String
smile s = s ++ " :)"

say :: String -> Spec String
say = spec

add :: Int -> Spec Int
add = spec

and' :: Mut t
and' = nop

to :: Int -> Mut Int
to y x = spec (x + y)

display :: Conv Int String
display = spec . show

result :: Term String String
result = ("result: " ++)

compute :: Double -> Spec Double
compute = spec

plus :: ArgMut Double Double
plus i p = spec (i + p)

please :: Term Double Double
please = id

percent :: Mut Double
percent = nop

of' :: Double -> Mut Double
of' d p = spec (d * (p / 100))

type ArgMut a t = t -> Conv a t

stack :: Spec [Int]
stack = spec []

push :: ArgMut Int [Int]
push s v = spec (v : s)

pop :: Mut [Int]
pop = spec . tail

end :: Term [Int] [Int]
end = id
