module Wave where

import Codec.Wav
import Data.Array.Unboxed
import Data.Audio
import Data.Int

audioData :: SampleData Int16
audioData = array (1, 100) [(i, fromIntegral i * 100) | i <- [1..100]]

audio :: Audio Int16
audio = Audio { sampleRate = 44100, channelNumber = 1, sampleData = audioData }

main = do
	exportFile "wave.wav" audio

