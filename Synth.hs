{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Synth where

import Midi
import Play
import Rendering
import Wave

import Types
import Instruments

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
	&= verbosity


main :: IO ()
main = do
	Args{..} <- cmdArgs argsDefs

	(tones, dur) <- loadMidi input
	let
		samples = map (\(time, tone) -> timeShift time $ render tone) tones
		finalAudio = sumSamples samples dur
		output = if outputArg /= "" then outputArg else
			replaceExtension input "wav"
	saveWave output finalAudio
	
	when playArg $ play finalAudio

