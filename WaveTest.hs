module WaveTest (testsWave) where

import Types
import Wave

import qualified Data.Vector.Unboxed as V

import Test.HUnit
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

instance (Arbitrary a, V.Unbox a) => Arbitrary (V.Vector a) where
	arbitrary = V.fromList `fmap` arbitrary

newtype SampleTest = SampleTest Sample deriving (Show)
instance Arbitrary SampleTest where
	arbitrary = SampleTest `fmap` choose (-1.0, 1.0)

newtype FrameStreamTest = FrameStreamTest FrameStream deriving (Show)
instance Arbitrary FrameStreamTest where
	arbitrary = FrameStreamTest `fmap` arbitrary

prop_adddouble_value (FrameStreamTest stream1) (FrameStreamTest stream2) =
	(fst stream1 == 0) ==>
	let sum = addStream stream1 stream2 in fst sum == 0


testsWave = testGroup "Wave tests" [
	testProperty "some wave property" prop_adddouble_value
	]

