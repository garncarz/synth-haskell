{-|
Module for loading a content of a MIDI file as timed absolute frequency tones.
-}
module Midi where

import Types

import Codec.Midi as Midi hiding (Time)

midi2realNotes :: Midi -- ^ Content of a MIDI file
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones
midi2realNotes midi = filterNotes realTimedTrack where
	track = (tracks $ toSingleTrack midi) !! 0
	realTimedTrack = toRealTime (timeDiv midi) $ toAbsTime track

-- | Extracts non-percussive unmuted tones.
filterNotes :: [(Time, Message)] -- ^ Timed MIDI messages
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones with duration
filterNotes [] = []
filterNotes ((time, msg):rest)
	| isNoteOn msg && velocity msg > 0 && channel msg /= 9 = (time, Tone {
		pitch = absoluteFrequency $ key msg,
		duration = findNextNoteOffTime (key msg) (channel msg) rest - time,
		volume = fromIntegral (velocity msg) / 128}):filterNotes rest
	| otherwise = filterNotes rest

-- | Finds when a given note stops.
findNextNoteOffTime :: Key -> Channel -> [(Time, Message)] -> Time
findNextNoteOffTime _ _ [] = error "no next NoteOff"
findNextNoteOffTime key chan ((time, msg):rest)
	| isNoteOn msg && Midi.key msg == key && channel msg == chan &&
		velocity msg == 0 = time
	| isNoteOff msg && Midi.key msg == key && channel msg == chan = time
	| otherwise = findNextNoteOffTime key chan rest

absoluteFrequency :: Key -- ^ 69 (A in MIDI)
	-> Frequency -- ^ 440\.0 (Hz)
absoluteFrequency key = 440 * 2 ** ((fromIntegral key - 69) / 12)

songDuration :: [(Time, Tone)]
	-> Duration
songDuration notes = (+) 1 $ maximum $ map noteEnding notes where
	noteEnding note = (fst note) + (duration $ snd note)


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

