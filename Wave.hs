module Wave where

import Types

import Codec.Wav
import Data.Array.Unboxed
import Data.Audio

emptyTrack :: Duration -> FrameStream
emptyTrack dur = array (0, len) [(i, 0) | i <- range(0, len)] where
	len = round $ dur * realSamplingRate

sumStreams :: FrameStream -> FrameStream -> FrameStream
sumStreams a1 a2 = accum (+) a1 (assocs a2)

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

