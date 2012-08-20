module Synth where

import Midi
import Play
import Rendering
import Types
import Wave

main :: IO ()
main = do
	(tones, dur) <- loadMidi "input.midi"
	let
		samples = map (\(time, tone) -> timeShift time $
			render (nice triangle) tone) tones
		empty = emptyTrack dur
		finalAudio = foldl1 sumStreams (empty:samples)
	saveWave "output.wav" finalAudio
	play finalAudio samplingRate

