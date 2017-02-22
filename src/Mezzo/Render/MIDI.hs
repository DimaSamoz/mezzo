{-# LANGUAGE TypeInType, GADTs #-}

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
    ( renderMusic )
    where

import Mezzo.Model

import Codec.Midi

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

-- | Start playing the specified 'MidiNote'.
keyDown :: MidiNote -> MidiEvent
keyDown n = (start n, NoteOn {channel = 0, key = noteNum n, velocity = vel n})

-- | Stop playing the specified 'MidiNote'.
keyUp :: MidiNote -> MidiEvent
keyUp n = (start n + noteDur n, NoteOn {channel = 0, key = noteNum n, velocity = 0})

-- | Play the specified 'MidiNote'.
playNote :: MidiNote -> MidiTrack
playNote k = [keyDown k, keyUp k]

-- | Play a rest of the specified duration.
playRest :: Ticks -> MidiTrack
playRest dur = playNote MidiNote {noteNum = 60, vel = 0, start = 0, noteDur = dur}

-- | Merge two parallel MIDI tracks.
(>+<) :: MidiTrack -> MidiTrack -> MidiTrack
m1 >+< m2 = removeTrackEnds $ m1 `merge` m2

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
    , timeDiv = TicksPerBeat 480
    , tracks =
        [ [ (0, ChannelPrefix 0)
          , (0, TrackName " Grand Piano  ")
          , (0, InstrumentName "GM Device  1")
          , (0, TimeSignature 2 4 24 8)
          , (0, KeySignature 0 0)
          ]
        ++ mel
        ++ [ (0, TrackEnd) ]
        ]
    }

-- | Convert a 'Music' piece into a 'MidiTrack'.
musicToMidi :: Music m -> MidiTrack
musicToMidi (Note root dur) =
    playNote MidiNote {noteNum = prim root, vel = 100, start = 0, noteDur = durToTicks dur}
musicToMidi (Rest dur) = playRest (durToTicks dur)
musicToMidi (m1 :|: m2) = musicToMidi m1 ++ musicToMidi m2
musicToMidi (m1 :-: m2) = musicToMidi m1 >+< musicToMidi m2

-- | Create a MIDI file with the specified name and track.
createMidi :: FilePath -> MidiTrack -> IO ()
createMidi f notes = exportFile f $ midiSkeleton notes

-- | Create a MIDI file with the specified path and composition.
renderMusic :: FilePath -> Music m -> IO ()
renderMusic f m = createMidi f (musicToMidi m)
