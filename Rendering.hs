module Rendering where

import Instruments
import Types

import qualified Data.Vector.Unboxed as V

render :: Tone -> FrameStream
render tone = (0, V.fromList [vol * sampler instrument freq dur (time frame)
		| frame <- [0..frames]])
	where
		frames = ceiling $ realSamplingRate * dur
		freq = pitch tone; dur = duration tone; vol = volume tone
		time frame = fromIntegral frame / realSamplingRate
		instrument = instrumentFor tone

