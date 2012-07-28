module Wave where

import Codec.Midi hiding (Time, exportFile)
import Codec.Wav hiding (importFile)
import Data.Array.Unboxed
import Data.Audio
import Data.Int
import Data.List

import Sound.Sox.Play
import Sound.Sox.Signal.List
import Sound.Sox.Option.Format hiding (sampleRate)

type Frequency = Double
type Time = Double
type Duration = Time
type FrameNr = Int
type DiscreteSample = Int16
type SampleFunc = Frequency -> FrameNr -> Sample
type SampleFuncReal = Frequency -> Time -> Sample
type FrameStream = UArray FrameNr Sample


samplingRate = 1000
--samplingRate = 10
realSamplingRate = fromIntegral samplingRate
maxDiscreteVal = (fromIntegral (maxBound :: DiscreteSample) :: Sample) / 10


sinusoid :: SampleFunc
sinusoid freq frame = sin(time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate

niceSinusoid :: SampleFunc
niceSinusoid freq frame = exp(-5 * time) * sin (time * 2 * pi * freq)
	where time = fromIntegral frame / realSamplingRate


niceSinusoidReal :: SampleFuncReal
niceSinusoidReal freq time = exp(-5 * time) * sin (time * 2 * pi * freq)

noteSampleAtTime :: Time -> (Time, Frequency) -> Sample
noteSampleAtTime now (start, freq) = if start < now - 2 || start > now then 0
	else niceSinusoidReal freq now

totalSampleAtTime :: [(Time, Frequency)] -> Time -> Sample
totalSampleAtTime notes now = sum $ map (noteSampleAtTime now) notes


render :: (FrameNr -> Sample) -> Duration -> FrameStream
render func dur = array (0, frames)
	[(frame, func frame) | frame <- range(0, frames)]
	where frames = ceiling $ realSamplingRate * dur

renderTotal :: [(Time, Frequency)] -> [(FrameNr, Sample)]
renderTotal notes = (map
	(\(time, frame) -> let sample = totalSampleAtTime notes time in
	(frame, sample)) [(time, frame) | frame <- range(0, 70 * samplingRate),
	let time = fromIntegral frame / realSamplingRate])


emptyEmbracing :: FrameStream -> FrameStream -> FrameStream
emptyEmbracing a1 a2 = array (min3, max3) [(i, 0) | i <- range(min3, max3)]
	where
		min3 = min (fst b1) (fst b2); max3 = max (snd b1) (snd b2)
		b1 = bounds a1; b2 = bounds a2

sumUnboxed :: FrameStream -> FrameStream -> FrameStream
sumUnboxed a1 a2 = a4 where
	empty = emptyEmbracing a1 a2
	a3 = accum (+) empty (assocs a1)
	a4
		| a1Embracing = accum (+) a1 (assocs a2)
		| a2Embracing = accum (+) a2 (assocs a1)
		| otherwise = accum (+) a3 (assocs a2)
	a1Embracing = fst b1 <= fst b2 && snd b1 >= snd b2
	a2Embracing = fst b2 <= fst b1 && snd b2 >= snd b1
	b1 = bounds a1; b2 = bounds a2


emptyTrack :: FrameNr -> FrameStream
emptyTrack len = array (0, len) [(i, 0) | i <- range(0, len)]

sumStreams :: FrameStream -> FrameStream -> FrameStream
sumStreams a1 a2 = accum (+) a1 (assocs a2)


timeShift :: Time -> FrameStream -> FrameStream
timeShift shift a1 = ixmap (min2, max2) (\i -> i - shiftFr) a1 where
	(min1, max1) = bounds a1; min2 = min1 + shiftFr; max2 = max1 + shiftFr
	shiftFr = round $ shift * realSamplingRate


unbox :: [(FrameNr, Sample)] -> FrameStream
unbox stream1 = array (min, max) stream1 where
	min = 0; max = fst $ last stream1


discreteSamples :: FrameStream -> SampleData DiscreteSample
discreteSamples = amap (\e -> round $ maxDiscreteVal * e)


audio :: FrameStream -> Audio DiscreteSample
audio audioData = Audio { sampleRate = samplingRate, channelNumber = 1,
	sampleData = discreteSamples audioData }


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
	realShifted = fromIntegral shifted / 384 / 3

absoluteFrequency :: Key -> Frequency
absoluteFrequency key = 440 * 2 ** ((fromIntegral key - 69) / 12)


simpleData :: FrameStream -> [Int16]
simpleData stream = map (\(_, sample) -> round $ maxDiscreteVal * sample)
	(assocs stream)




loadMidi filename = do
	fileContent <- importFile filename
	let midi = case fileContent of
		Left err -> error $ "o-o " ++ err
		Right midi -> midi
	let
		absoluteNotes = sortBy (\(t1, _) (t2, _) -> compare t1 t2)
			$ concat $ map midi2realNotes $ tracks midi
		samples = map (\(time, freq) -> timeShift time $
			render (niceSinusoid freq) 1) absoluteNotes
		empty = emptyTrack $ 300 * samplingRate
		finalAudio = foldl1 sumStreams (empty:samples)
		
		--lasts = map (\s -> s ! (snd $ bounds s)) samples
		--checkpoints = fst (partition (\(frameNr, _) ->
		--	frameNr `mod` samplingRate == 0) (assocs finalAudio))
		
		--total = renderTotal absoluteNotes
		--checkpoints = fst (partition (\(frameNr, _) ->
		--	frameNr `mod` samplingRate == 0) total)
	--print lasts
	--print $ last total
	--print checkpoints
	--print total
	exportFile "04-prokrastinacni_orgie.wav" (audio finalAudio)
	simple Sound.Sox.Signal.List.put none samplingRate (simpleData finalAudio)
	--exportFile "prochazka.wav" (audio (unbox total))


main = do
	--exportFile "wave.wav" audio
	loadMidi "04-prokrastinacni_orgie.midi"

