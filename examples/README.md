# Mezzo example compositions
This folder contains some example compositions written in the Mezzo EDSL. The pieces have different musical styles and showcase the various composition techniques that Mezzo supports. Smaller (single-file) pieces are in the Other folder, while larger (multi-file) compositions have folders of their own.

> Note: running `stack build` on this directory will take a long time and produce very large log files. Therefore it is recommended to launch a GHCi session without building the project with `stack ghci --no-build`, then manually loading the desired composition and calling `main` to render out the file.

## [Four-part contrapuntal chorale](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Other/Harmonisation.hs)
A simple four-part chorale in strict counterpoint. First, the individual voices are written using the melody construction syntax, which are then harmonically composed together into a score. The maximum musical strictness is selected by switching to the `strict` rule set.

## [Happy Birthday song](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Other/HappyBirthday.hs)
A demonstration of the schematic chord progression syntax. The melody and accompaniment are written separately and combined using the `melAccomp` function: this takes a melody and a progression, and composes them into a musical piece. Note that `start` and `prog` are called internally in the `melAccomp` function. The key and time signature of the progression are given as score attributes.

## [Johann Sebastian Bach – *Prelude in C Major*](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Other/Bach.hs)
This example showcases the advantages of embedding Mezzo into a functional language. Bach's piece is structurally and rhythmically very repetitive: the bars only differ by the five pitches that appear in them. To exploit this structural similarity, we can define a function which takes the five pitches and generates an entire bar of the composition. The full piece can therefore be constructed by calls to this bar-generating function. Note that despite the complex type-level computation involved, Haskell can infer the type of the `bar` function.

## [Frederic Chopin – *Prelude in C Minor*](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Chopin.hs)
Chopin's piece consists of a sequence of chords of various sizes, types and durations. Mezzo's symbolic chord input can handle all but the most complicated chords, making the structure of the piece very clear. The example also makes use of *modular composition*: the two parts of the piece are written in two different modules, pre-rendered into independent sections. As the third part is a repeat of the second part, the corresponding score is simply used twice in the `renderScores` function.

## [Ludwig van Beethoven – *Für Elise*](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/FurElise.hs)
A structurally diverse piano piece with thematic episodes and variations, complex melody, rhythm and harmony, showcasing most of the composition features implemented in Mezzo. Again, the composition is written in separate modules which speeds up compilation (the whole piece doesn't need to be recompiled if a small change is made in a module), and lets us repeat the `theme` several times in the composition.

## [Steve Reich – *Clapping Music*](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Other/ClappingMusic.hs)
An example of the score-level transformations which allow users to manipulate existing (rule-checked) scores with various operations such as transposition and repetition. This piece consists of a repetitive beat played by two voices, with the second voice shifting by one eight every four repetitions. This creates an interesting, complex pattern, despite the rhythmic similarity of the two voices.

## [Nikolai Rimsky-Korsakov – *Flight of the Bumblebee* (excerpt)](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Other/Bumblebee.hs)
A short excerpt of the piece, showcasing further score-level transformations such as reprises, voltas, etc. Most of the chromatic scales are implemented as the repetition and transposition of a "singleton" score.

## [First species counterpoint rule set](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Other/FirstSpecies.hs)
An example of a custom rule set for first species counterpoint. Mezzo's flexible rule sets allow users to change type-checking behaviour with term-level function arguments (score attributes), and even define custom rules which can be fully integrated with the type-level model of music.
