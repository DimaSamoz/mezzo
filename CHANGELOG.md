# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

<!-- ## [Unreleased](https://github.com/DimaSamoz/mezzo/compare/v0.3.1...HEAD) -->

## [0.3.1 - 2017-10-07](https://github.com/DimaSamoz/mezzo/releases/tag/v0.3.1)
### Added
- Live playback on the terminal: Mezzo compositions can now be played back live from the terminal using an external midi synthesiser (e.g. [SimpleSynth](http://notahat.com/simplesynth/)) and [Euterpea](http://www.euterpea.com/).
- Empty cadences: you can use `end` as a cadential phrase to compose a chord progression without a cadence.
- Score transformation: you can use the functions such as `delay` and `transpose` to manipulate music at the MIDI track level. While this allows for more flexibility, composition of `Score`s is not rule-checked, so there is no static guarantee that the resulting music will sound good.

### Changed
- The melody constructor `play` has been renamed to `start`.

### Fixed
- Readme typos and rule set changes in examples.
- Clashes with Prelude's `min` function.

## [0.3.0 - 2017-06-16](https://github.com/DimaSamoz/mezzo/releases/tag/v0.3.0)
### Added
- Chord progressions: create chord progressions following the conventions of functional harmony.
- Dyads: 2-note chords (thirds, fourths, fifths and octaves), together with their doubled versions.
- Scores: break up a piece into sections that can be compiled and combined separately, and specify various attributes of the music, such as tempo, title, key and time signature.
- Triplets: play three notes in the time of two.
- Rule sets: customise the musical rules checked by the library. Includes three strictness levels, from no rule-checking to strict counterpoint.
- Several small and large examples.

### Changed
- Better error messages, showing the pitches that cause errors.
- Faster rule-checking performance.

## 0.2.0 - 2017-03-17
Initial [Hackage](https://hackage.haskell.org/package/mezzo) release.
