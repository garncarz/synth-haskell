module Play where

import Types

import Data.Array.Unboxed
import Data.Int
import Sound.Sox.Play
import Sound.Sox.Signal.List
import Sound.Sox.Option.Format
import System.Exit

simpleData :: FrameStream -> [Int16]
simpleData stream = map (\(_, sample) -> round $ maxVolume * sample)
	(assocs stream)

play :: FrameStream -> Int -> IO () -- FIXME Int
play audio samplingRate = do
	simple put none samplingRate (simpleData audio) `catch`
		\err -> do putStrLn $ show err; exitFailure
	return ()


