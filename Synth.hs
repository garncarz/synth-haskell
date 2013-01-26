{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Synth where

import Instruments
import Midi
import Play
import Rendering
import Types
import Wave

import Control.Monad
import Data.Maybe
import System.Console.CmdArgs
import qualified System.FilePath as Path

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
		toneStreams = map (\tone -> (tone, render tone)) $ uniqueTones tones
		findStream tone = fromJust $ lookup tone toneStreams
		streams = map (\(time, tone) -> timeShift time $ findStream tone) tones
		finalAudio = sumStreams streams dur
		output = if outputArg /= "" then outputArg else
			Path.replaceExtension input "wav"
	saveWave output finalAudio
	
	when playArg $ play finalAudio

