module Midi where

import Types

import Codec.Midi hiding (Time)
import Data.List

midi2realNotes :: Int -> Track Ticks -> [(Time, Tone)]
midi2realNotes tempo track = absoluteNotes where
	absoluteTimeNotes = makeAbsoluteTimes track 0 tempo
	notesOn = fst (partition (\(_, m) -> isNoteOn m && velocity m > 0)
		absoluteTimeNotes)
	relativeNotes = map (\(time, note) -> (time, key note)) notesOn
	absoluteNotes = map (\(time, key) -> (time, Tone {
		pitch = absoluteFrequency key, duration = 1, volume = 1}))
		relativeNotes

makeAbsoluteTimes :: [(Ticks, Message)] -> FrameNr -> Int -> [(Time, Message)]
makeAbsoluteTimes [] _ tempo = []
makeAbsoluteTimes ((time, m):rest) shift tempo = (realShifted, m):rest2 where
	rest2 = makeAbsoluteTimes rest shifted tempo; shifted = time + shift
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
		notes = sortTimed . concat $ map (midi2realNotes tempo) $ tracks midi
		songDuration = (+) 2 . fst $ last notes
	return (notes, songDuration)

