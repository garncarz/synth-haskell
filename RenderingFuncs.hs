module RenderingFuncs where

import Types

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

