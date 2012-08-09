module Play where

import Types

import Control.Exception
import Data.Array.Unboxed
import Data.Int
import Prelude hiding (catch)
import Sound.Sox.Play as Play
import Sound.Sox.Signal.List as SignalList
import Sound.Sox.Option.Format as Option
import System.Exit

simpleData :: FrameStream -> [Int16]
simpleData stream = map (\(_, sample) -> round $ maxVolume * sample)
	(assocs stream)

play :: FrameStream -> Int -> IO () -- FIXME Int
play audio samplingRate = do
	Play.simple SignalList.put Option.none samplingRate (simpleData audio)
		`catch` \err -> do putStrLn $ show (err :: IOException); exitFailure
	return ()

