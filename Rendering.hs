module Rendering where

import Types

import Data.Audio
import Data.Array.Unboxed

sinusoid :: SampleFunc
sinusoid freq _ frame = sin(time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate

niceSinusoid :: SampleFunc
niceSinusoid freq dur frame = exp(-2 * time / dur) * sin (time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate


render :: SampleFunc -> Tone -> FrameStream
render func tone = array (0, frames)
	[(frame, vol * (func freq dur frame)) | frame <- range(0, frames)]
	where frames = ceiling $ realSamplingRate * dur;
		freq = pitch tone; dur = duration tone; vol = volume tone

