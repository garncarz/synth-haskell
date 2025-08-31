module MidiTest (testsMidi) where

import Midi
import Codec.Midi (Message(..))

import Test.HUnit
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

newtype Key = Key Int deriving (Show)

instance Arbitrary Key where
	arbitrary = elements [Key k | k <- [1..300]]

almostEq x y = abs(x - y) < 0.01

prop_absoluteFrequency_ord (Key key1) (Key key2) = compare key1 key2 == compare
	(absoluteFrequency key1) (absoluteFrequency key2)

test_absoluteFrequency_vals = do
	assertEqual "A4" 440 (absoluteFrequency 69)
	assertBool "C4" $ almostEq 261.63 (absoluteFrequency 60)

test_findNextNoteOffTime_noNoteOff = do
	let result = findNextNoteOffTime 60 0 10.0 []  -- No messages, should use default
	assertEqual "No NoteOff should give default duration" 11.0 result

test_findNextNoteOffTime_withNoteOff = do
	let msgs = [(15.0, NoteOff 0 60 127)]  
	let result = findNextNoteOffTime 60 0 10.0 msgs
	assertEqual "With NoteOff should return NoteOff time" 15.0 result


testsMidi = testGroup "MIDI tests" [
	testProperty "absoluteFrequency ordering" prop_absoluteFrequency_ord,
	testCase "some absoluteFrequency values" test_absoluteFrequency_vals,
	testCase "findNextNoteOffTime with no NoteOff" test_findNextNoteOffTime_noNoteOff,
	testCase "findNextNoteOffTime with NoteOff" test_findNextNoteOffTime_withNoteOff
	]

