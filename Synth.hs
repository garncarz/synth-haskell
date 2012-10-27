{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Synth where

import Midi
import Play
import Rendering
import Types
import Wave

import System.Console.CmdArgs

data Args = Args {input :: String, output :: String}
	deriving (Show, Data, Typeable)

argsDefs = Args {
	input = def &= typ "INPUT" &= argPos 0 &= opt "input.midi",
	output = def &= typ "OUTPUT" &= argPos 1 &= opt "output.wav" }
	&= program "synth"


main :: IO ()
main = do
	Args{..} <- cmdArgs argsDefs

	(tones, dur) <- loadMidi input
	let
		samples = map (\(time, tone) -> timeShift time $
			render (nice square) tone) tones
		empty = emptyTrack dur
		finalAudio = foldl1 sumStreams (empty:samples)
	saveWave output finalAudio
	play finalAudio samplingRate

