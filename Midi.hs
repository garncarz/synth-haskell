module Midi where

import Types

import Codec.Midi hiding (Time)

midi2realNotes :: Track Ticks -> [(Time, Frequency)]
midi2realNotes track = absoluteNotes where
	absoluteTimeNotes = makeAbsoluteTimes track 0
	notesOn = fst (partition (\(_, m) -> isNoteOn m && velocity m > 0)
		absoluteTimeNotes)
	relativeNotes = map (\(time, note) -> (time, key note)) notesOn
	absoluteNotes = map (\(time, key) -> (time, absoluteFrequency key))
		relativeNotes

makeAbsoluteTimes :: [(Ticks, Message)] -> FrameNr -> [(Time, Message)]
makeAbsoluteTimes [] _ = []
makeAbsoluteTimes ((time, m):rest) shift = (realShifted, m):rest2 where
	rest2 = makeAbsoluteTimes rest shifted; shifted = time + shift
	realShifted = fromIntegral shifted / 384 / 3  -- FIXME

absoluteFrequency :: Key -> Frequency
absoluteFrequency key = 440 * 2 ** ((fromIntegral key - 69) / 12)


loadMidi :: String -> IO ()
loadMidi filename = do
	fileContent <- importFile filename
	let midi = case fileContent of
		Left err -> error $ "o-o " ++ err
		Right midi -> midi
	let notes = sortBy (\(t1, _) (t2, _) -> compare t1 t2)
			$ concat $ map midi2realNotes $ tracks midi
	return notes

