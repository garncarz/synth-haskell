module Rendering where

import Types

import Data.Audio
import Data.Array.Unboxed

renderingFuncs = [sine, saw, square, triangle] :: [SampleFunc]

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


sawVol :: VolumeFunc
sawVol _ time = (\x -> x - (fromIntegral . floor) x) $ time * 10

endLinefadeOut :: VolumeFunc
endLinefadeOut dur time = if fromFading < 0 then 1 else fading where
	fading = 1 - (fromFading / fadingTime)
	fromFading = time - ((1 - fadingPart) * dur)
	fadingTime = fadingPart * dur
	fadingPart = 0.2

expFadeOut :: VolumeFunc
expFadeOut dur time = exp $ log 0.05 * time / dur

cosFadeOut :: VolumeFunc
cosFadeOut dur time = cos $ acos 0 * time / dur


nice :: SampleFunc -> SampleFunc
nice func freq dur time = func freq dur time * cosFadeOut dur time


instrumentFunc :: Tone -> SampleFunc
instrumentFunc tone = renderingFuncs !! (instrument tone `mod`
	length renderingFuncs)


render :: Tone -> FrameStream
render tone = array (0, frames)
	[(frame, vol * nice func freq dur (time frame)) | frame <- range(0, frames)]
	where
		frames = ceiling $ realSamplingRate * dur
		freq = pitch tone; dur = duration tone; vol = volume tone
		time frame = fromIntegral frame / realSamplingRate
		func = instrumentFunc tone

