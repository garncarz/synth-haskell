{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Synth where

import Midi
import Play
import Rendering
import Types
import Wave

import Control.Monad
import System.Console.CmdArgs

data Args = Args {input :: String, output :: String, play :: Bool }
	deriving (Show, Data, Typeable)

argsDefs = Args {
	input = def &= typ "INPUT" &= argPos 0 &= opt "input.midi",
	output = def &= typ "OUTPUT" &= argPos 1 &= opt "output.wav",
	play = False &= typ "BOOL" }
	&= program "synth"


main :: IO ()
main = do
	Args{..} <- cmdArgs argsDefs

	(tones, dur) <- loadMidi input
	putStrLn $ "MIDI file " ++ input ++ " loaded, " ++ show (length tones) ++
		" tones."
	
	let
		samples = map (\(time, tone) -> timeShift time $
			render (nice sine) tone) tones
		empty = emptyTrack dur
		finalAudio = foldl1 sumStreams (empty:samples)
	saveWave output finalAudio
	putStrLn $ "WAVE file " ++ output ++ " saved."
	
	when play $ Play.play finalAudio samplingRate

