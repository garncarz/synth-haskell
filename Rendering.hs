module Rendering where

import Instruments
import Types

import qualified Data.Vector.Unboxed as V

render :: Tone -> FrameStream
render tone = (0, V.fromList [genFunc frame | frame <- [0..frames]])
	where
		genFunc frame = vol * sampler instrument freq dur (time frame)
		frames = ceiling $ realSamplingRate * dur
		freq = pitch tone; dur = duration tone; vol = volume tone
		time frame = fromIntegral frame / realSamplingRate
		instrument = instrumentFor tone

