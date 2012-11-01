module Rendering where

import Instruments
import Types

import Data.Array.Unboxed

render :: Tone -> FrameStream
render tone = array (0, frames)
	[(frame, vol * sampler instrument freq dur (time frame))
		| frame <- range(0, frames)]
	where
		frames = ceiling $ realSamplingRate * dur
		freq = pitch tone; dur = duration tone; vol = volume tone
		time frame = fromIntegral frame / realSamplingRate
		instrument = instrumentFor tone

