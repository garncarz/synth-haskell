module Midi where

import Types

import Codec.Midi as Midi hiding (Time)
import Data.List


midi2realNotes :: Midi -> Int -> [(Time, Tone)]
midi2realNotes midi tempo = sortTimed . concat . map filterNotes .
	map (makeAbsoluteTimes tempo 0)	$ tracks midi

filterNotes :: [(Time, Message)] -> [(Time, Tone)]
filterNotes [] = []
filterNotes ((time, msg):rest)
	| isNoteOn msg && velocity msg > 0 && channel msg /= 9 = (time, Tone {
		pitch = absoluteFrequency $ key msg,
		duration = findNextNoteOffTime (key msg) (channel msg) rest - time,
		volume = fromIntegral (velocity msg) / 128}):filterNotes rest
	| otherwise = filterNotes rest

findNextNoteOffTime :: Key -> Channel -> [(Time, Message)] -> Time
findNextNoteOffTime _ _ [] = error "no next NoteOff"
findNextNoteOffTime key chan ((time, msg):rest)
	| isNoteOn msg && Midi.key msg == key && channel msg == chan &&
		velocity msg == 0 = time
	| isNoteOff msg && Midi.key msg == key && channel msg == chan = time
	| otherwise = findNextNoteOffTime key chan rest

makeAbsoluteTimes :: Int -> FrameNr -> [(Ticks, Message)] -> [(Time, Message)]
makeAbsoluteTimes _ _ [] = []
makeAbsoluteTimes tempo shift ((time, m):rest) = (realShifted, m):rest2 where
	rest2 = makeAbsoluteTimes tempo shifted rest; shifted = time + shift
	realShifted = fromIntegral shifted / (fromIntegral tempo) / 3  -- FIXME

absoluteFrequency :: Key -> Frequency
absoluteFrequency key = 440 * 2 ** ((fromIntegral key - 69) / 12)

sortTimed :: Ord a => [(a, b)] -> [(a, b)]
sortTimed = sortBy (\(t1, _) (t2, _) -> compare t1 t2)


loadMidi :: String -> IO ([(Time, Tone)], Duration)
loadMidi filename = do
	fileContent <- importFile filename
	let midi = case fileContent of
		Left err -> error $ "o-o " ++ err
		Right midi -> midi
	let
		tempoMidi (TicksPerBeat tpb) = tpb
		tempo = tempoMidi (timeDiv midi)
		notes = midi2realNotes midi tempo
		songDuration = (+) 2 . fst $ last notes
	return (notes, songDuration)

