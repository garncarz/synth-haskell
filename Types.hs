module Types where

import Codec.Midi hiding (Time)
import Data.Array.Unboxed
import Data.Audio
import Data.Int

type Frequency = Double

-- | Time in seconds, meant mainly for a time positioning
type Time = Double

-- | Time in seconds
type Duration = Time

type Volume = Double
type FrameNr = Int
type DiscreteSample = Int16
type SampleFunc = Frequency -> Duration -> Time -> Sample
type VolumeFunc = Duration -> Time -> Sample
type FrameStream = UArray FrameNr Sample

data Tone = Tone {
	pitch :: Frequency,
	duration :: Duration,
	volume :: Volume,
	instrument :: Preset,
	channel :: Channel } deriving (Eq, Show, Read)

maxVolume = (fromIntegral (maxBound :: DiscreteSample) :: Sample) / 10

samplingRate = 11000 :: Int
realSamplingRate = fromIntegral samplingRate :: Double
