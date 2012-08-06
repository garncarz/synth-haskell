module Types where

import Data.Array.Unboxed
import Data.Audio
import Data.Int

type Frequency = Double
type Time = Double
type Duration = Time
type FrameNr = Int
type DiscreteSample = Int16
type SampleFunc = Frequency -> FrameNr -> Sample
type SampleFuncReal = Frequency -> Time -> Sample
type FrameStream = UArray FrameNr Sample

maxVolume = (fromIntegral (maxBound :: DiscreteSample) :: Sample) / 10

samplingRate = 1000 :: Int
realSamplingRate = (fromIntegral samplingRate) :: Double


