import Midi

import Test.HUnit
import Test.QuickCheck

newtype Key = Key Int deriving (Show)

instance Arbitrary Key where
	arbitrary = elements [Key k | k <- [1..300]]

almostEq x y = abs(x - y) < 0.01

prop_absoluteFrequency_ord (Key key1) (Key key2) = compare key1 key2 == compare
	(absoluteFrequency key1) (absoluteFrequency key2)

test_absoluteFrequency_vals = do
	assertEqual "A4" 440 (absoluteFrequency 69)
	assertBool "C4" $ almostEq 261.63 (absoluteFrequency 60)

