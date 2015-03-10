# Haskell MIDI to WAVE synthesizer

## Installation

```
$ cabal install
```

Installs dependencies, takes some time.
*(GHC size is ~800 MB, so you wouldn't expect it to include libraries for
command line arguments processing, regexps, unit testing etc., right?)*
Also compiles the project and creates an executable `~/.cabal/bin/synth`.

## Run

```
$ synth input.midi [output.wav]
```

Runs for ages.
*Since rendering functions are pure Haskell functions with fixed results based just on arguments
with no side effects whatsoever, you don't really expect GHC to cache them by any means, do you?*

## Examples

* [Eins](https://garncarz.github.io/synth-haskell/04-prokrastinacni_orgie.ogg) ([source MIDI](https://garncarz.github.io/synth-haskell/04-prokrastinacni_orgie.midi))
* [Zwei](https://garncarz.github.io/synth-haskell/06-dechove_nastroje_v_d-dur.ogg) ([source MIDI](https://garncarz.github.io/synth-haskell/06-dechove_nastroje_v_d-dur.mid))
