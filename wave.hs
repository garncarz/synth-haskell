module Wave where

import Codec.Wav
import Data.Array.Unboxed
import Data.Audio
import Data.Int
import Data.List

type Frequency = Double
type Time = Double
type Duration = Time
type FrameNr = Int
type DiscreteSample = Int16
type SampleFunc = Frequency -> FrameNr -> Sample
type FrameStream = UArray FrameNr Sample


samplingRate = 44100
realSamplingRate = fromIntegral samplingRate
maxDiscreteVal = (fromIntegral (maxBound :: DiscreteSample) :: Sample) / 2


sinusoid :: SampleFunc
sinusoid freq frame = sin(time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate

niceSinusoid :: SampleFunc
niceSinusoid freq frame = exp(-5 * time) * sin (time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate

render :: (FrameNr -> Sample) -> Duration -> FrameStream
render func dur = array (0, frames)
	[(frame, func frame) | frame <- range(0, frames)]
	where frames = ceiling $ realSamplingRate * dur


emptyEmbracing :: FrameStream -> FrameStream -> FrameStream
emptyEmbracing a1 a2 = array (min3, max3) [(i, 0) | i <- range(min3, max3)]
	where
		min3 = min (fst b1) (fst b2); max3 = max (snd b1) (snd b2)
		b1 = bounds a1; b2 = bounds a2

sumUnboxed :: FrameStream -> FrameStream -> FrameStream
sumUnboxed a1 a2 = a4 where
	empty = emptyEmbracing a1 a2
	a3 = accum (+) empty (assocs a1)
	a4 = accum (+) a3 (assocs a2)

timeShift :: Time -> FrameStream -> FrameStream
timeShift shift a1 = ixmap (min2, max2) (\i -> i - shiftFr) a1 where
	(min1, max1) = bounds a1; min2 = min1 + shiftFr; max2 = max1 + shiftFr
	shiftFr = round $ shift * realSamplingRate


discreteSamples :: FrameStream -> SampleData DiscreteSample
discreteSamples a = amap (\e -> round $ maxDiscreteVal * e) a


audioData :: SampleData DiscreteSample
audioData = discreteSamples $ sumUnboxed (render (niceSinusoid 100) 4)
	(timeShift 0.5 $ render (niceSinusoid 100) 0.2)

audio :: Audio DiscreteSample
audio = Audio { sampleRate = samplingRate, channelNumber = 1,
	sampleData = audioData }


main = do
	exportFile "wave.wav" audio

