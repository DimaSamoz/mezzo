# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## 0.3.0 - 2017-06-10
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
