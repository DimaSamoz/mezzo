
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Render.Transform
-- Description :  Score-level transformations
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions and combinators for transforming Scores. Allows for more flexibility,
-- but no static correctness guarantees.
--
-----------------------------------------------------------------------------

module Mezzo.Render.Transform
    (transpose, delay, (><), (+++), flatten, reprise, cascade, scale, volta)
    where

import Mezzo.Render.Score
import Mezzo.Render.MIDI

import Codec.Midi hiding (key, Key)
import qualified Codec.Midi as CM (key, Key)
import Prelude hiding (min)
import Control.Arrow (first, second)

-- | Transpose a single MIDI message by the given number of semitones.
transposeMessage :: Int -> Message -> Message
transposeMessage t (NoteOff ch key vel) = NoteOff ch (key + t) vel
transposeMessage t (NoteOn ch key vel) = NoteOn ch (key + t) vel
transposeMessage _ m = m

-- | Transpose a score by the given number of semitones.
transpose :: Int -> Score -> Score
transpose n = map (second (transposeMessage n))

-- | Delay a score by the given number of ticks (60ths of a thirty-second note).
delay :: Ticks -> Score -> Score
delay ticks (n@(_, NoteOn{}) : rest) =
    [ (0, NoteOn {channel = 0, CM.key = 60, velocity = 0})
    , (ticks, NoteOn {channel = 0, CM.key = 60, velocity = 0})
    ] ++ n : rest
delay ticks (m : rest) = m : delay ticks rest

-- | Remove the attribute MIDI messages in the score header.
stripHeader :: Score -> Score
stripHeader [] = []
stripHeader (n@(_, NoteOn{}) : rest) = n : rest
stripHeader (_ : rest) = stripHeader rest

-- | Concatenation of scores.
(+++) :: Score -> Score -> Score
s1 +++ s2 = s1 ++ stripHeader s2

-- | Flatten a list of scores into a single score.
flatten :: [Score] -> Score
flatten = foldr (+++) []

-- | Repeat a score the given number of times.
reprise :: Int -> Score -> Score
reprise n = flatten . replicate n

-- | Repeat the score, successively applying the function to each repetition.
-- cascade 4 f s = s +++ f s +++ f (f s) +++ f (f (f s))
cascade :: Int -> (Score -> Score) -> Score -> Score
cascade n f = flatten . take n . iterate f

-- | Create a scale of the given length and interval between scores.
scale :: Int -> Int -> Score -> Score
scale l i = cascade l (transpose i)

-- | Repeat the score with the different endings at each repetition.
volta :: Score -> [Score] -> Score
volta s vs = flatten $ map (s +++) vs
