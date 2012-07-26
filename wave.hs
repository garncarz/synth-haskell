module Wave where

import Codec.Wav
import Data.Array.Unboxed
import Data.Audio
import Data.Int
import Data.List

type Frequency = Double
type Duration = Double
type FrameNr = Int
type UnboxedSample = Int16
type SampleFunc = Frequency -> FrameNr -> Sample
type FrameData = (FrameNr, Sample)
type FrameStream = [FrameData]

samplingRate = 44100
realSamplingRate = fromIntegral samplingRate
maxUnboxedVal = (fromIntegral (maxBound :: UnboxedSample) :: Double) / 2


sinusoid :: SampleFunc
sinusoid freq frame = sin(time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate

niceSinusoid :: SampleFunc
niceSinusoid freq frame = exp(-5 * time) * sin (time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate

render :: (FrameNr -> Sample) -> Duration -> FrameStream
render func dur = [(frame, func frame) | frame <- [1 .. frames]]
	where frames = ceiling $ realSamplingRate * dur


addFrameStreams :: FrameStream -> FrameStream -> FrameStream
addFrameStreams a b = [(i, frame i a + frame i b) | i <- ix] where
	aix = map fst a; bix = map fst b; ix = nub $ aix ++ bix
	frame i f = let e = find (\e -> fst e == i) f in case e of
		Just (_, sample) -> sample
		Nothing -> 0


unbox :: FrameStream -> SampleData UnboxedSample
unbox stream = array (1, length stream) $
	map (\(frameNr, sample) -> (frameNr, round $ maxUnboxedVal * sample)) stream


audioData :: SampleData UnboxedSample
audioData = unbox $ addFrameStreams (render (niceSinusoid 100) 0.1)
	(render (niceSinusoid 100) 0.1)

audio :: Audio UnboxedSample
audio = Audio { sampleRate = samplingRate, channelNumber = 1,
	sampleData = audioData }


main = do
	exportFile "wave.wav" audio

