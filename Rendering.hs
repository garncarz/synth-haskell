module Rendering where

import Types

import Data.Audio
import Data.Array.Unboxed

sine :: SampleFunc
sine freq _ time = sin $ time * 2 * pi * freq

saw :: SampleFunc
saw freq _ time = (\x -> 2 * x - 1) . (\x ->  x - (fromIntegral . floor) x) $
	freq * time

square :: SampleFunc
square freq dur time = signum $ sine freq dur time

triangle :: SampleFunc
triangle freq _ time = (\x -> 2 * x - 1) . abs .
	(\x ->  x - (fromIntegral . floor) x) $ freq * time

nice :: SampleFunc -> SampleFunc
nice func freq dur time = exp (-2 * time / dur) * (func freq dur time)


render :: SampleFunc -> Tone -> FrameStream
render func tone = array (0, frames)
	[(frame, vol * (func freq dur $ time frame)) | frame <- range(0, frames)]
	where
		frames = ceiling $ realSamplingRate * dur
		freq = pitch tone; dur = duration tone; vol = volume tone
		time frame = fromIntegral frame / realSamplingRate

