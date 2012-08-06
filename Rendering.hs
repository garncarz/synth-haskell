module Rendering where

import Types

import Data.Audio
import Data.Array.Unboxed

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

