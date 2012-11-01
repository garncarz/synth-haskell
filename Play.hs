module Play where

import Types

import Control.Exception
import Data.Array.Unboxed
import Prelude hiding (catch)
import Sound.Sox.Play as Play
import Sound.Sox.Signal.List as SignalList
import Sound.Sox.Option.Format as Option
import System.Exit

simpleData :: FrameStream -> [DiscreteSample]
simpleData stream = map (\(_, sample) -> round $ maxVolume * sample)
	(assocs stream)

play :: FrameStream -> IO ()
play audio = do
	Play.simple SignalList.put Option.none samplingRate (simpleData audio)
		`catch` \err -> do print (err :: IOException); exitFailure
	return ()

