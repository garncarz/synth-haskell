{-|
Module for loading a content of a MIDI file as timed absolute frequency tones.
-}
module Midi where

import Types hiding (channel, preset)
import qualified Types

import Codec.Midi as Midi
import Data.IntervalMap.Strict (IntervalMap, Interval(..), insert, singleton,
	containing)
import Data.List hiding (insert)
import System.Console.CmdArgs.Verbosity
import System.Time.Utils

midi2realTones :: Midi -- ^ Content of a MIDI file
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones
midi2realTones midi = extractNotes realTimedTrack presets where
	track = head (tracks $ toSingleTrack midi)
	realTimedTrack = toRealTime (timeDiv midi) $ toAbsTime track
	presets = presetsIntervals realTimedTrack

-- | Extracts non-percussive unmuted tones.
extractNotes :: [(Time, Message)] -- ^ Timed MIDI messages
	-> [IntervalMap Time Preset]
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones with duration
extractNotes [] _ = []
extractNotes ((time, msg):rest) presets
	| isNoteOn msg && velocity msg > 0 && channel msg /= 9 = (time, Tone {
		pitch = absoluteFrequency $ key msg,
		duration = findNextNoteOffTime (key msg) (channel msg) rest - time,
		volume = fromIntegral (velocity msg) / 128,
		Types.preset = presetAt chan time presets,
		Types.channel = chan }) : extractNotes rest presets
	| otherwise = extractNotes rest presets
	where chan = channel msg

-- | Finds when a given note stops.
findNextNoteOffTime :: Key -> Channel -> [(Time, Message)] -> Time
findNextNoteOffTime _ _ [] = error "no next NoteOff"
findNextNoteOffTime keyToStop chan ((time, msg):rest)
	| isNoteOn msg && key msg == keyToStop && channel msg == chan &&
		velocity msg == 0 = time
	| isNoteOff msg && key msg == keyToStop && channel msg == chan = time
	| otherwise = findNextNoteOffTime keyToStop chan rest


defaultPreset = 1 :: Preset

presetIntervals' :: [(Time, Message)] -> Channel -> IntervalMap Time Preset
presetIntervals' track chan = pileUp aether $
	(fromNowOnwards . crystalize . extract) track where
	
	extract = filter (\(_, msg) -> isProgramChange msg && channel msg == chan)
	crystalize = map (\(time, msg) -> (time, preset msg))

	fromNowOnwards = map (\(time, preset) -> (IntervalCO time infinity, preset))
	infinity = 1 / 0

	pileUp = foldl (\pile (interval, preset) -> insert interval preset pile)
	aether = singleton infiniteInterval defaultPreset
	infiniteInterval = OpenInterval (-infinity) infinity

presetsIntervals :: [(Time, Message)] -> [IntervalMap Time Preset]
presetsIntervals track = map (presetIntervals' track) [0..16]

presetAt :: Channel -> Time -> [IntervalMap Time Preset] -> Preset
presetAt chan time db = snd . last $ containing (db !! chan) time


absoluteFrequency :: Key -- ^ 69 (A in MIDI)
	-> Frequency -- ^ 440\.0 (Hz)
absoluteFrequency key = 440 * 2 ** ((fromIntegral key - 69) / 12)


songDuration :: [(Time, Tone)]
	-> Duration
songDuration notes = (+) 1 $ maximum $ map noteEnding notes where
	noteEnding note = fst note + duration (snd note)

totalDuration :: [(Time, Tone)] -> Duration
totalDuration = sum . map (duration . snd)

uniqueTones :: [(Time, Tone)] -> Int
uniqueTones = length . nub . map snd

uniqueTonesDuration :: [(Time, Tone)] -> Duration
uniqueTonesDuration = sum . map duration . nub . map snd


loadMidi :: String  -- ^ File name
	-> IO ([(Time, Tone)], Duration) -- ^ Timed absolute frequency tones
		-- and a duration of the whole song
loadMidi filename = do
	fileContent <- importFile filename
	let
		midi = case fileContent of
			Left err -> error $ "Error loading MIDI: " ++ err
			Right midi -> midi
		tones = midi2realTones midi
		dur = songDuration tones
	
	whenNormal $ putStrLn $ let showTime = renderSecs . round in
		"MIDI file " ++ filename ++ " loaded, "
		++ showTime dur ++ " playing time, "
		++ (showTime . totalDuration) tones ++ " total time for "
		++ (show . length) tones ++ " tones ("
		++ (show . uniqueTones) tones ++ " unique, "
		++ (showTime . uniqueTonesDuration) tones ++ ")."
	
	return (tones, dur)

