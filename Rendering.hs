module Rendering where

import Types

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

harmonicsVolumes :: [[Volume]]
harmonicsVolumes = [
	[1, -0.5, 0.2],
	[-1, -0.2, -0.5],
	[0.5, 1],
	[-1, 0.8, 0.6, -0.1]
	]

harmonics :: [Volume] -> SampleFunc -> SampleFunc
harmonics vols func freq dur time = sum $ map (\(vol, mul) ->
	vol * func (mul * freq) dur time) $ zip vols [1..]


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

nice :: VolumeFunc
nice = cosFadeOut


instrumentFunc :: Tone -> SampleFunc
instrumentFunc tone = renderingFuncs !! (instrument tone `mod`
	length renderingFuncs)

instrumentHarmonics :: Tone -> [Volume]
instrumentHarmonics tone = harmonicsVolumes !! (channel tone `mod`
	length harmonicsVolumes)


render :: Tone -> FrameStream
render tone = array (0, frames)
	[(frame, vol * nice dur (time frame) *
		harmonics vols func freq dur (time frame))
		| frame <- range(0, frames)]
	where
		frames = ceiling $ realSamplingRate * dur
		freq = pitch tone; dur = duration tone; vol = volume tone
		time frame = fromIntegral frame / realSamplingRate
		func = instrumentFunc tone
		-- func = sine
		vols = instrumentHarmonics tone

