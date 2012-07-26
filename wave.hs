module Wave where

import Codec.Wav
import Data.Array.Unboxed
import Data.Audio
import Data.Int

type Frequency = Float
type Duration = Float
type Time = Int
type SampleT = Int16
type SampleFunc = Frequency -> Time -> SampleT

samplingRate = 44100
maxVal = 2 ^ 16 / 2


toReal :: Int -> Float
toReal x = read (show x)


sinusoid :: SampleFunc
sinusoid freq time = floor $ maxVal * sin(realTime * 2 * pi * freq)
	where realTime = toReal time / toReal samplingRate

niceSinusoid :: SampleFunc
niceSinusoid freq time = floor $ maxVal * exp(-5 * realTime) * sin
	(realTime * 2 * pi * freq)
	where realTime = toReal time / toReal samplingRate

sample :: (Time -> SampleT) -> Duration -> SampleData SampleT
sample func dur = array (1, samples) [(time, func time) | time <- [1..samples]]
	where samples = truncate $ toReal samplingRate * dur


audioData :: SampleData SampleT
audioData = sample (niceSinusoid 100) 10

audio :: Audio SampleT
audio = Audio { sampleRate = samplingRate, channelNumber = 1,
	sampleData = audioData }


main = do
	exportFile "wave.wav" audio

