module Wave where

import Types

import qualified Codec.Wav as Wav
import qualified Data.Array.Unboxed as A
import Data.Audio
import qualified Data.Vector.Unboxed as V
import qualified System.Console.CmdArgs.Verbosity as Args


addStream :: FrameStream -> FrameStream -> FrameStream
addStream (0, base) (start, added) = (0, V.concat [pre, sum, post]) where
	end = start + V.length added
	pre = V.slice 0 start base
	venue = V.slice start (end - start) base
	post = V.slice end (V.length base - end) base
	sum = V.zipWith (+) venue added

sumStreams :: [FrameStream] -> Duration -> FrameStream
sumStreams streams dur = foldl addStream emptyStream streams where
	emptyStream = (0, V.replicate len 0)
	len = round $ dur * realSamplingRate

timeShift :: Time -> FrameStream -> FrameStream
timeShift shift (startFrame, samples) = (startFrame + shiftFr, samples)
	where shiftFr = round $ shift * realSamplingRate

discreteSamples :: FrameStream -> V.Vector DiscreteSample
discreteSamples (_, samples) =
	V.map (\sample -> round $ maxVolume * sample) samples

discreteSamplesUnboxed :: FrameStream -> SampleData DiscreteSample
discreteSamplesUnboxed stream = A.array (0, frames)
	[(frame, samples V.! frame) | frame <- [0..frames]]
	where
		samples = discreteSamples stream
		frames = (V.length . snd) stream - 1

audio :: FrameStream -> Audio DiscreteSample
audio audioData = Audio { sampleRate = samplingRate, channelNumber = 1,
	sampleData = discreteSamplesUnboxed audioData }

saveWave :: String -> FrameStream -> IO ()
saveWave filename stream = do
	Wav.exportFile filename (audio stream)
	Args.whenNormal $ putStrLn $ "WAVE file " ++ filename ++ " saved."

