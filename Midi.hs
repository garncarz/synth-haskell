{-|
Module for loading a content of a MIDI file as timed absolute frequency tones.
-}
module Midi where

import Types hiding (channel)
import qualified Types

import Codec.Midi as Midi hiding (Time)
import Data.IntervalMap.Strict (IntervalMap, Interval(..), insert, singleton,
	containing)
import Data.List hiding (insert)

midi2realNotes :: Midi -- ^ Content of a MIDI file
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones
midi2realNotes midi = extractNotes realTimedTrack instruments where
	track = head (tracks $ toSingleTrack midi)
	realTimedTrack = toRealTime (timeDiv midi) $ toAbsTime track
	instruments = instrumentsIntervals realTimedTrack

-- | Extracts non-percussive unmuted tones.
extractNotes :: [(Time, Message)] -- ^ Timed MIDI messages
	-> [IntervalMap Time Preset]
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones with duration
extractNotes [] _ = []
extractNotes ((time, msg):rest) instruments
	| isNoteOn msg && velocity msg > 0 && channel msg /= 9 = (time, Tone {
		pitch = absoluteFrequency $ key msg,
		duration = findNextNoteOffTime (key msg) (channel msg) rest - time,
		volume = fromIntegral (velocity msg) / 128,
		instrument = instrumentAt chan time instruments,
		Types.channel = chan }) : extractNotes rest instruments
	| otherwise = extractNotes rest instruments
	where chan = channel msg

-- | Finds when a given note stops.
findNextNoteOffTime :: Key -> Channel -> [(Time, Message)] -> Time
findNextNoteOffTime _ _ [] = error "no next NoteOff"
findNextNoteOffTime key chan ((time, msg):rest)
	| isNoteOn msg && Midi.key msg == key && channel msg == chan &&
		velocity msg == 0 = time
	| isNoteOff msg && Midi.key msg == key && channel msg == chan = time
	| otherwise = findNextNoteOffTime key chan rest


defaultPreset = 1 :: Preset

instrumentIntervals' :: [(Time, Message)] -> Channel -> IntervalMap Time Preset
instrumentIntervals' track chan = pileUp aether $
	(fromNowOnwards . crystalize . extract) track where
	
	extract = filter (\(_, msg) -> isProgramChange msg && channel msg == chan)
	crystalize = map (\(time, msg) -> (time, preset msg))

	fromNowOnwards = map (\(time, preset) -> (IntervalCO time infinity, preset))
	infinity = 1 / 0

	pileUp = foldl (\pile (interval, preset) -> insert interval preset pile)
	aether = singleton infiniteInterval defaultPreset
	infiniteInterval = OpenInterval (-infinity) infinity

instrumentsIntervals :: [(Time, Message)] -> [IntervalMap Time Preset]
instrumentsIntervals track = map (instrumentIntervals' track) [0..16]

instrumentAt :: Channel -> Time -> [IntervalMap Time Preset] -> Preset
instrumentAt chan time db = snd . last $ containing (db !! chan) time


absoluteFrequency :: Key -- ^ 69 (A in MIDI)
	-> Frequency -- ^ 440\.0 (Hz)
absoluteFrequency key = 440 * 2 ** ((fromIntegral key - 69) / 12)


songDuration :: [(Time, Tone)]
	-> Duration
songDuration notes = (+) 1 $ maximum $ map noteEnding notes where
	noteEnding note = fst note + duration (snd note)

totalDuration :: [(Time, Tone)] -> Duration
totalDuration = sum . map (duration . snd)


loadMidi :: String  -- ^ File name
	-> IO ([(Time, Tone)], Duration) -- ^ Timed absolute frequency tones
		-- and a duration of the whole song
loadMidi filename = do
	fileContent <- importFile filename
	let midi = case fileContent of
		Left err -> error $ "o-o " ++ err
		Right midi -> midi
	let notes = midi2realNotes midi
	return (notes, songDuration notes)

