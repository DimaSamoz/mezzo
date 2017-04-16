{-# LANGUAGE StandaloneDeriving, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mezzo.Render.Score
-- Description :  Score building
-- Copyright   :  (c) Dima Szamozvancev
-- License     :  MIT
--
-- Maintainer  :  ds709@cam.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Combinators for building scores: 'Music' values with global composition
-- attributes such as tempo or key signature.
--
-----------------------------------------------------------------------------

module Mezzo.Render.Score
    ( -- * Scores and attributes
      Attributes (..)
    , Score (..)
    , defAttributes
    , getTimeSig
    , getKeySig
      -- * Score builders
    , score
    , setTitle
    , setTempo
    , setTimeSig
    , setKeySig
    , withMusic
    )
    where

import Mezzo.Model
import Mezzo.Compose.Harmony
import Mezzo.Compose.Builder

import Codec.Midi hiding (key, Key)
import qualified Codec.Midi as CM (key, Key)
import qualified GHC.TypeLits as GT

-------------------------------------------------------------------------------
-- Attributes
-------------------------------------------------------------------------------

-- | Datatype containing MIDI attributes of a Mezzo composition.
data Attributes t k =
    (Primitive t, Primitive k, ScoreAtt t, ScoreAtt k)
    => Attributes
    { title :: String               -- ^ The title of the composition.
    , tempo :: Tempo                -- ^ The tempo of the composition in BPM.
    , timeSignature :: TimeSig t    -- ^ The time signature of the composition.
    , keySignature :: KeyS k        -- ^ The key signature of the composition.
    }

deriving instance Show (Attributes t k)

-- | Default attributes: "Composition" in C major in common time, with tempo 120 BPM.
defAttributes :: Attributes 4 (Key C Natural MajorMode)
defAttributes = Attributes
    { title = "Composition"
    , tempo = 120
    , timeSignature = quadruple
    , keySignature = c_maj
    }

-- | A type encapsulating every 'Music' composition with their MIDI attributes.
data Score = forall m t k. Score (Attributes t k) (Music (Sig :: Signature t k) m)

-------------------------------------------------------------------------------
-- Builders
-------------------------------------------------------------------------------

-- Score attribute specifier: uses the default attributes.
score :: Spec (Attributes 4 (Key C Natural MajorMode))
score = spec defAttributes

-- | Sets the title of the composition.
setTitle :: AMut String (Attributes t k)
setTitle atts titl = spec (atts {title = titl})

-- | Sets the tempo of the composition.
setTempo :: AMut Tempo (Attributes t k)
setTempo atts temp = spec (atts {tempo = temp})

-- | Sets the time signature of the composition.
setTimeSig :: (Primitive t', ScoreAtt t') => AConv (TimeSig t') (Attributes t k) (Attributes t' k)
setTimeSig Attributes{..} ts = spec (Attributes title tempo ts keySignature)

-- | Sets the key signature of the composition.
setKeySig :: (Primitive k', ScoreAtt k') => AConv (KeyS k') (Attributes t k) (Attributes t k')
setKeySig Attributes{..} ks = spec (Attributes title tempo timeSignature ks)

-- | Sets the music content of the score.
withMusic :: ATerm (Music (Sig :: Signature t k) m) (Attributes t k) Score
withMusic = Score

-- | Get the time signature MIDI message.
getTimeSig :: Attributes t k -> Message
getTimeSig Attributes{timeSignature = t} = getAtt t

-- | Get the key signature MIDI message.
getKeySig :: Attributes t k -> Message
getKeySig Attributes{keySignature = k} = getAtt k

-- | Class for types that can be converted into score attribute MIDI messages.
class ScoreAtt a where
    -- | Get the MIDI message corresponding to a type-level attribute.
    getAtt :: proxy a -> Message

instance ScoreAtt 2 where getAtt t = TimeSignature 2 2 24 8
instance ScoreAtt 3 where getAtt t = TimeSignature 3 2 24 8
instance ScoreAtt 4 where getAtt t = TimeSignature 4 2 24 8

instance ScoreAtt (Key C Flat    MajorMode) where getAtt k = KeySignature (-7) 0
instance ScoreAtt (Key G Flat    MajorMode) where getAtt k = KeySignature (-6) 0
instance ScoreAtt (Key D Flat    MajorMode) where getAtt k = KeySignature (-5) 0
instance ScoreAtt (Key A Flat    MajorMode) where getAtt k = KeySignature (-4) 0
instance ScoreAtt (Key E Flat    MajorMode) where getAtt k = KeySignature (-3) 0
instance ScoreAtt (Key B Flat    MajorMode) where getAtt k = KeySignature (-2) 0
instance ScoreAtt (Key F Natural MajorMode) where getAtt k = KeySignature (-1) 0
instance ScoreAtt (Key C Natural MajorMode) where getAtt k = KeySignature  0 0
instance ScoreAtt (Key G Natural MajorMode) where getAtt k = KeySignature  1 0
instance ScoreAtt (Key D Natural MajorMode) where getAtt k = KeySignature  2 0
instance ScoreAtt (Key A Natural MajorMode) where getAtt k = KeySignature  3 0
instance ScoreAtt (Key E Natural MajorMode) where getAtt k = KeySignature  4 0
instance ScoreAtt (Key B Natural MajorMode) where getAtt k = KeySignature  5 0
instance ScoreAtt (Key F Sharp   MajorMode) where getAtt k = KeySignature  6 0
instance ScoreAtt (Key C Sharp   MajorMode) where getAtt k = KeySignature  7 0

instance ScoreAtt (Key A Flat    MinorMode) where getAtt k = KeySignature (-7) 1
instance ScoreAtt (Key E Flat    MinorMode) where getAtt k = KeySignature (-6) 1
instance ScoreAtt (Key B Flat    MinorMode) where getAtt k = KeySignature (-5) 1
instance ScoreAtt (Key F Natural MinorMode) where getAtt k = KeySignature (-4) 1
instance ScoreAtt (Key C Natural MinorMode) where getAtt k = KeySignature (-3) 1
instance ScoreAtt (Key G Natural MinorMode) where getAtt k = KeySignature (-2) 1
instance ScoreAtt (Key D Natural MinorMode) where getAtt k = KeySignature (-1) 1
instance ScoreAtt (Key A Natural MinorMode) where getAtt k = KeySignature  0 1
instance ScoreAtt (Key E Natural MinorMode) where getAtt k = KeySignature  1 1
instance ScoreAtt (Key B Natural MinorMode) where getAtt k = KeySignature  2 1
instance ScoreAtt (Key F Sharp   MinorMode) where getAtt k = KeySignature  3 1
instance ScoreAtt (Key C Sharp   MinorMode) where getAtt k = KeySignature  4 1
instance ScoreAtt (Key G Sharp   MinorMode) where getAtt k = KeySignature  5 1
instance ScoreAtt (Key D Sharp   MinorMode) where getAtt k = KeySignature  6 1
instance ScoreAtt (Key A Sharp   MinorMode) where getAtt k = KeySignature  7 1

instance {-# OVERLAPPABLE #-} GT.TypeError (GT.Text "The key signature is invalid.")
    => ScoreAtt (Key pc acc mode) where
    getAtt = undefined
