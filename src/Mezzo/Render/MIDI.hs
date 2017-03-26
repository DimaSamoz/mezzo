
-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Render.MIDI
-- Description :  MIDI exporting
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for exporting Mezzo compositions into MIDI files.
-- Skeleton code by Stephen Lavelle.
--
-----------------------------------------------------------------------------

module Mezzo.Render.MIDI
    ( renderMusic, musicToMidi )
    where

import Mezzo.Model
import Mezzo.Compose (_th, _si, _ei, _qu, _ha, _wh)

import Codec.Midi hiding (key)
import qualified Codec.Midi as CM (key)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A MIDI representation of a musical note.
data MidiNote = MidiNote
    { noteNum :: Int        -- ^ MIDI number of a note (middle C is 60).
    , vel     :: Velocity   -- ^ Performance velocity of the note.
    , start   :: Ticks      -- ^ Relative start time of the note.
    , noteDur :: Ticks      -- ^ Duration of the note.
    } deriving Show

-- | A MIDI event: a MIDI message at a specific timestamp.
type MidiEvent = (Ticks, Message)

-- | A sequence of MIDI events.
type MidiTrack = Track Ticks

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Play a MIDI note with the specified duration and default velocity.
midiNote :: Int -> Ticks -> MidiNote
midiNote root dur = MidiNote {noteNum = root, vel = 100, start = 0, noteDur = dur}

midiRest :: Ticks -> MidiNote
midiRest dur = MidiNote {noteNum = 60, vel = 0, start = 0, noteDur = dur}

-- | Start playing the specified 'MidiNote'.
keyDown :: MidiNote -> MidiEvent
keyDown n = (start n, NoteOn {channel = 0, CM.key = noteNum n, velocity = vel n})

-- | Stop playing the specified 'MidiNote'.
keyUp :: MidiNote -> MidiEvent
keyUp n = (start n + noteDur n, NoteOn {channel = 0, CM.key = noteNum n, velocity = 0})

-- | Play the specified 'MidiNote'.
playNote :: Int -> Ticks -> MidiTrack
playNote root dur = map ($ midiNote root dur) [keyDown, keyUp]

-- | Play a rest of the specified duration.
playRest :: Ticks -> MidiTrack
playRest dur = map ($ midiRest dur) [keyDown, keyUp]

-- | Merge two parallel MIDI tracks.
(><) :: MidiTrack -> MidiTrack -> MidiTrack
m1 >< m2 = removeTrackEnds $ m1 `merge` m2

-- | Convert a 'Dur' to 'Ticks'.
durToTicks :: Primitive d => Dur d -> Ticks
durToTicks d = prim d * 60 -- 1 Mezzo tick (a 32nd note) ~ 60 MIDI ticks

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

-- | A basic skeleton of a MIDI file.
midiSkeleton :: MidiTrack -> Midi
midiSkeleton mel = Midi
    { fileType = MultiTrack
    , timeDiv = TicksPerBeat 120
    , tracks =
        [ [ (0, ChannelPrefix 0)
          , (0, TrackName " Grand Piano  ")
          , (0, InstrumentName "GM Device  1")
          , (0, TimeSignature 4 2 24 8)
          , (0, KeySignature 0 0)
          ]
        ++ mel
        ++ [ (0, TrackEnd) ]
        ]
    }

-- | Convert a 'Music' piece into a 'MidiTrack'.
musicToMidi :: Music m -> MidiTrack
musicToMidi (m1 :|: m2) = musicToMidi m1 ++ musicToMidi m2
musicToMidi (m1 :-: m2) = musicToMidi m1 >< musicToMidi m2
musicToMidi (Note root dur) = playNote (prim root) (durToTicks dur)
musicToMidi (Rest dur) = playRest (durToTicks dur)
musicToMidi (Chord c d) = foldr1 (><) notes
    where notes = map (`playNote` durToTicks d) $ prim c
musicToMidi (Progression ts p) = foldr1 (++) chords
    where chords = (toChords <$> init (prim p)) ++ [cadence (last (prim p))]
          toChords :: [Int] -> MidiTrack
          toChords = concat . replicate (prim ts) . foldr1 (><) . map (`playNote` durToTicks _qu)
          cadence :: [Int] -> MidiTrack
          cadence = foldr1 (><) . map (`playNote` durToTicks _wh)
musicToMidi (Homophony m a) = musicToMidi m >< musicToMidi a

-- | Create a MIDI file with the specified name and track.
createMidi :: FilePath -> MidiTrack -> IO ()
createMidi f notes = exportFile f $ midiSkeleton notes

-- | Create a MIDI file with the specified path and composition.
renderMusic :: FilePath -> Music m -> IO ()
renderMusic f m = createMidi f (musicToMidi m)
