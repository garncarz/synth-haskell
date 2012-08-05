module Play where

import Types

import Data.Int
import Sound.Sox.Play
import Sound.Sox.Signal.List
import Sound.Sox.Option.Format

simpleData :: FrameStream -> [Int16]
simpleData stream = map (\(_, sample) -> round $ maxVolume * sample)
	(assocs stream)

play :: FrameStream -> Int -> IO () -- FIXME Int
play audio samplingRate = 
	simple Sound.Sox.Signal.List.put none samplingRate (simpleData audio)

