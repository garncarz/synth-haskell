module Play where

import Types
import Wave

import Control.Exception
import qualified Data.Vector.Unboxed as V
import Prelude hiding (catch)
import Sound.Sox.Play as Play
import Sound.Sox.Signal.List as SignalList
import Sound.Sox.Option.Format as Option
import System.Exit

play :: FrameStream -> IO ()
play audio = do
	Play.simple SignalList.put Option.none samplingRate (simpleData audio)
		`catch` \err -> do print (err :: IOException); exitFailure
	return ()
	where simpleData stream = V.toList (discreteSamples stream)

