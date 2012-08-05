module Synth where

import Midi
import Play
import Rendering
import Types
import Wave

main :: IO ()
main = do
	notes <- loadMidi "input.midi"
	let samples = map (\(time, freq) -> timeShift time $
			render (niceSinusoid freq) 1) notes
		empty = emptyTrack $ 300 * samplingRate
		finalAudio = foldl1 sumStreams (empty:samples)
	saveWave "output.wav" finalAudio
	play finalAudio samplingRate

