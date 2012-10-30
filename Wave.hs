module Wave where

import Types

import Codec.Wav
import Data.Array.Unboxed
import Data.Audio

sumSamples :: [FrameStream] -> Duration -> FrameStream
sumSamples samples dur = accumArray (+) 0 (0, len) (concatMap assocs samples)
	where len = round $ dur * realSamplingRate

timeShift :: Time -> FrameStream -> FrameStream
timeShift shift a1 = ixmap (min2, max2) (\i -> i - shiftFr) a1 where
	(min1, max1) = bounds a1; min2 = min1 + shiftFr; max2 = max1 + shiftFr
	shiftFr = round $ shift * realSamplingRate

discreteSamples :: FrameStream -> SampleData DiscreteSample
discreteSamples = amap (\e -> round $ maxVolume * e)

audio :: FrameStream -> Audio DiscreteSample
audio audioData = Audio { sampleRate = samplingRate, channelNumber = 1,
	sampleData = discreteSamples audioData }

saveWave :: String -> FrameStream -> IO ()
saveWave filename stream = exportFile filename (audio stream)

