module Instruments where

import RenderingFuncs
import Types

between :: Ord a => a -> a -> a -> Bool
between min max elem = elem >= min && elem <= max

simple :: [Volume] -> SampleFunc
simple vols freq dur time = cosFadeOut dur time *
	harmonics vols sine freq dur time

-- fictive harmonics values
instruments :: [Instrument]
instruments = [
	Instrument { name = "piano",
		isPreset = between 0 8,
		sampler = simple [1, -0.5, 0.2] },
	Instrument { name = "steel",
		isPreset = between 9 16,
		sampler = simple [0.5, 1] },
	Instrument { name = "organ",
		isPreset = between 17 21,
		sampler = simple [-0.5, 0.3, -0.25] },
	Instrument { name = "concertina",
		isPreset = between 22 24,
		sampler = simple [-0.8, 0.2, -0.4, 0.02] },
	Instrument { name = "ac. guitar",
		isPreset = between 25 26,
		sampler = simple [1, 0.2, -0.4] },
	Instrument { name = "el. guitar",
		isPreset = between 27 32,
		sampler = simple [-1, 0.8, 0.6, -0.1] },
	Instrument { name = "bass",
		isPreset = between 33 40,
		sampler = simple [-1, -0.2, -0.5] },
	Instrument { name = "strings",
		isPreset = \p -> between 41 52 p && notElem p [47, 48],
		sampler = simple [0.5, -0.3, 1] },
	defaultInstrument { name = "harp", isPreset = (==) 47 },
	defaultInstrument { name = "timpani", isPreset = (==) 48 },
	Instrument { name = "voice",
		isPreset = between 53 55,
		sampler = simple [1, -0.6, 0.4, -0.8] },
	Instrument { name = "orchestra hit",
		isPreset = (==) 56,
		sampler = simple [-1, 1, -1, 1] },
	Instrument { name = "brass",
		isPreset = between 57 64,
		sampler = simple [1, -0.4, -0.6, -0.8] },
	Instrument { name = "woodwind",
		isPreset = between 65 80,
		sampler = simple [-1, 0.8, 0.6, -0.4] },
	Instrument { name = "lead",
		isPreset = between 81 88,
		sampler = simple [0.2, -0.3, 0.6, -0.8] },
	Instrument { name = "pad",
		isPreset = between 89 96,
		sampler = simple [0.8, -0.3, 0.55, -0.2] },
	Instrument { name = "fx",
		isPreset = between 97 104,
		sampler = simple [0.33, -0.3, 0.44, -0.44] },
	Instrument { name = "exotic",
		isPreset = between 105 112,
		sampler = simple [-0.9, 0.3, 0.25, -0.4] },
	Instrument { name = "exotic drums",
		isPreset = between 113 119,
		sampler = simple [-1, -0.15, 0.6] },
	defaultInstrument { name = "crazy", isPreset = (<) 119 }
	]

defaultInstrument = head instruments :: Instrument

instrumentFor :: Tone -> Instrument
instrumentFor tone = if length approp > 0 then head approp
	-- else defaultInstrument
	else error $ "Unknown preset: " ++ (show . preset) tone
	where approp = filter (`isPreset` preset tone) instruments

