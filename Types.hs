module Types (Time, Preset,
	Frequency, Duration, Volume,
	FrameNr, Sample, DiscreteSample,
	SampleFunc, VolumeFunc, FrameStream,
	Instrument(..), Tone(..),
	maxVolume, samplingRate, realSamplingRate) where

import Codec.Midi
import Data.Array.Unboxed
import Data.Int

type Frequency = Double
type Duration = Time
type Volume = Double
type FrameNr = Int
type Sample = Double
type DiscreteSample = Int16
type SampleFunc = Frequency -> Duration -> Time -> Sample
type VolumeFunc = Duration -> Time -> Sample
type FrameStream = UArray FrameNr Sample

data Instrument = Instrument {
	name :: String,
	isPreset :: Preset -> Bool,
	sampler :: SampleFunc }

data Tone = Tone {
	pitch :: Frequency,
	duration :: Duration,
	volume :: Volume,
	preset :: Preset,
	channel :: Channel } deriving (Eq, Show, Read)

maxVolume = (fromIntegral (maxBound :: DiscreteSample) :: Sample) / 20

samplingRate = 44000 :: Int
realSamplingRate = fromIntegral samplingRate :: Double
