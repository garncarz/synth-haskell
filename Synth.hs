{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Synth where

import Midi
import Play
import Rendering
import Types
import Wave

import Control.Monad
import System.Console.CmdArgs
import System.FilePath

data Args = Args {input :: String, outputArg :: String, playArg :: Bool}
	deriving (Show, Data, Typeable)

argsDefs = Args {
	input = def &= typ "INPUT" &= argPos 0,
	outputArg = def &= typ "OUTPUT" &= argPos 1 &= opt "",
	playArg = False &= typ "BOOL" }
	&= program "synth"


main :: IO ()
main = do
	Args{..} <- cmdArgs argsDefs

	(tones, dur) <- loadMidi input
	putStrLn $ "MIDI file " ++ input ++ " loaded, " ++ show (length tones) ++
		" tones."
	
	let
		samples = map (\(time, tone) -> timeShift time $ render tone) tones
		empty = emptyTrack dur
		finalAudio = foldl1 sumStreams (empty:samples)
		output = if outputArg /= "" then outputArg else
			replaceExtension input "wav"
	saveWave output finalAudio
	putStrLn $ "WAVE file " ++ output ++ " saved."
	
	when playArg $ play finalAudio samplingRate

