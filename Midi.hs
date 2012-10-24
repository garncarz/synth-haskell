{-|
Module for loading a content of a MIDI file as timed absolute frequency tones.

TODO:

  - Adapt to variable tempo during a song. MIDI change tempo messages must be
    taken into account.
  
  - Fix computing a duration of a song.
-}
module Midi where

import Types

import Codec.Midi as Midi hiding (Time)
import Data.IntervalMap.Strict (IntervalMap, Interval(..), insert, singleton,
	containing)
import Data.List hiding (insert, lookup)
import Prelude hiding (lookup)

midi2realNotes :: Midi -- ^ Content of a MIDI file
	-> Int -- ^ Tempo of music
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones
midi2realNotes midi tempo = sortTimed . concat . map filterNotes .
	map (makeAbsoluteTimes tempo 0)	$ tracks midi

-- | Extracts non-percussive unmuted tones.
filterNotes :: [(Time, Message)] -- ^ Timed MIDI messages
	-> [(Time, Tone)] -- ^ Timed absolute frequency tones
filterNotes [] = []
filterNotes ((time, msg):rest)
	| isNoteOn msg && velocity msg > 0 && channel msg /= 9 = (time, Tone {
		pitch = absoluteFrequency $ key msg,
		duration = findNextNoteOffTime (key msg) (channel msg) rest - time,
		volume = fromIntegral (velocity msg) / 128}):filterNotes rest
	| otherwise = filterNotes rest

-- | Finds when a given note is muted.
findNextNoteOffTime :: Key -> Channel -> [(Time, Message)] -> Time
findNextNoteOffTime _ _ [] = error "no next NoteOff"
findNextNoteOffTime key chan ((time, msg):rest)
	| isNoteOn msg && Midi.key msg == key && channel msg == chan &&
		velocity msg == 0 = time
	| isNoteOff msg && Midi.key msg == key && channel msg == chan = time
	| otherwise = findNextNoteOffTime key chan rest


-- | TODO: Hide Ticks, determine /variable/ tempo from messages.
makeAbsoluteTimes :: Int -- ^ Tempo of music
	-> Ticks -- ^ How many MIDI time units to shift
	-> [(Ticks, Message)] -- ^ MIDI timed MIDI messages
	-> [(Time, Message)] -- ^ Shifted real timed MIDI messages
makeAbsoluteTimes _ _ [] = []
makeAbsoluteTimes tempo shift ((time, m):rest) = (realShifted, m):rest2 where
	rest2 = makeAbsoluteTimes tempo shifted rest; shifted = time + shift
	realShifted = fromIntegral shifted / (fromIntegral tempo) / 3  -- FIXME


tempoIntervals :: [(Ticks, Message)] -> IntervalMap Time Tempo
tempoIntervals track = pileUp aether $ map fromNowOnwards $ pureList track where
	pureList = nub . sortTimed . crystalize . filterTempos
	
	filterTempos = filter (\(_, msg) -> isTempo msg)
	isTempo (TempoChange _) = True; isTempo _ = False
	
	crystalize = map (\(tick, msg) -> (tick, tempo msg))
	tempo (TempoChange t) = t
	
	fromNowOnwards = \(tick, tempo) -> (IntervalCO (fromIntegral tick) infinity,
		tempo)
	infinity = 1 / 0
	
	aether = singleton infiniteInterval defaultTempo
	infiniteInterval = OpenInterval (-infinity) infinity
	
	pileUp = foldl (\pile (interval, tempo) -> insert interval tempo pile)


tempoAt :: Time -> IntervalMap Time Tempo -> Tempo
tempoAt time tempos = snd . last $ containing tempos time

test = do
	fileContent <- importFile "input.midi"
	let midi = case fileContent of
		Left err -> error $ "o-o " ++ err
		Right midi -> midi
	let intervals = tempoIntervals (concat $ tracks midi)
	print intervals
	print $ tempoAt (-10) intervals
	print $ tempoAt 10 intervals


absoluteFrequency :: Key -- ^ 69 (A in MIDI)
	-> Frequency -- ^ 440\.0 (Hz)
absoluteFrequency key = 440 * 2 ** ((fromIntegral key - 69) / 12)

-- | Sorts by the first element of a couple.
sortTimed :: Ord a => [(a, b)] -> [(a, b)]
sortTimed = sortBy (\(t1, _) (t2, _) -> compare t1 t2)


-- | TODO: Fix duration.
loadMidi :: String  -- ^ File name
	-> IO ([(Time, Tone)], Duration) -- ^ Timed absolute frequency tones
		-- and a duration of the whole song
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

