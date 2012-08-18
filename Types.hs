module Types where

import Data.Array.Unboxed
import Data.Audio
import Data.Int

type Frequency = Double
type Time = Double
type Duration = Time
type Volume = Double
type FrameNr = Int
type DiscreteSample = Int16
type SampleFunc = Frequency -> Duration -> FrameNr -> Sample
type FrameStream = UArray FrameNr Sample

data Tone = Tone {
	pitch :: Frequency,
	duration :: Duration,
	volume :: Volume} deriving (Eq, Show, Read)

maxVolume = (fromIntegral (maxBound :: DiscreteSample) :: Sample) / 10

samplingRate = 4000 :: Int
realSamplingRate = (fromIntegral samplingRate) :: Double

