# Mezzo

[![Build Status](https://travis-ci.org/DimaSamoz/mezzo.svg?branch=master)](https://travis-ci.org/DimaSamoz/mezzo )

*Mezzo* is a Haskell library and embedded domain-specific language for music description. Its novelty is in the fact that it can enforce various rules of music composition *statically*, that is, at compile-time. This effectively means that if you write "bad" music, your composition will not compile – think of it as a **very** strict spell-checker for music.

Note: the project is still very much work-in-progress.

## Getting started

This section explains how to install Mezzo and start using the library.

### Prerequisites

Mezzo is a Haskell library with only a few dependencies. The main requirement is GHC 8.0.2: the package uses the latest and greatest features of the Haskell type system so it needs the most up-to-date version of the compiler. If you're using [`stack`](https://www.stackage.org/lts-8.5), use the `lts-8.5` resolver (or higher).


### Installation

If using Cabal, run

```
cabal update
cabal install mezzo
```

If using Stack, you will need to add the package to your `extra-deps` in your `stack.yaml` (as Mezzo is not part of Stackage yet), and then add it normally to your `.cabal` file dependencies:

```cabal
extra-deps: [ mezzo-0.2.0.0 ]

build-depends: base >= 4.7 && < 5
             , mezzo
```

Build the file, and you should be good to go.


### First composition

Create a new project (e.g., with `stack new`) with a `Main` module. Type:

```haskell
import Mezzo

comp = play $ melody :| c :| d :| e :| f :>> g

main :: IO ()
main = renderMusic "comp.mid" comp
```

Save, build and execute (e.g., with `stack exec <project_name>`). You should get a `.mid` file in the project directory which looks something like this:

![First composition](/docs/comp1.png)

To test the correctness checking, change the `d` in `comp` to a `b`. You should see the following when you save the file:

![First error](/docs/err1.png)

## Composing in Mezzo

This section provides more detail on the syntax of Mezzo.

### Basic concepts

*Music description languages* are textual representations of musical pieces, used for note input or transcription. Most MDLs provide ways of inputting notes, rests and ways to combine these into melodies or chords. Mezzo additionally lets users input *chords* in their symbolic representation, as well as *chord progressions* in the future.

### Literal vs. builder style

Mezzo provides two main ways of creating musical values:

* **Literal style**: create musical values with explicit constructors of literal values. For example a C natural in octave 4 is written as `pitch _c _na _o4` and can be abbreviated to `_cn`. A quarter note (pitch with duration) is then written as `noteP (pitch _c _na _o4) _qu` or `noteP _cn _qu`. Literal values are prefixed with an underscore and can be combined using the constructors `pitch` and `noteP`.
* **Builder style**: create musical values by sequencing their attributes from left to right. This style is more concise, flexible and readable than literal style and is therefore preferred. Creating a middle C quarter note is `c qn`, an F sharp dotted sixteenth note in the 5th octave is `fs sn'` or `f sharp sn'`. Similarly, a C major chord would be `c maj qc`, a B flat diminished seventh in first inversion with dotted half duration would be `b flat dim7 inv hc'`.

The rest of this guide uses the builder style.

### Primitives

#### Notes

Notes are given by their pitch and duration. In builder style, every pitch has an explicit value, consisting of three parts:

* **Pitch class**: one of `c`, `d`, `e`, `f`, `g`, `a` or `b`; determines the position of the note in the octave, a white key on the keyboard.
* **Accidental**: a suffix of the pitch class, one of `f` (flat) or `s` (sharp). Natural accidentals are not specified, so `c` means C natural. Accidentals can also be written out as a separate attribute (`c sharp qn`), or even repeated (for example, double sharps: `c sharp sharp qn` or `cs sharp qn`).
* **Octave**: the last component of the value. The default octave is 4, this is unmarked. Lower octaves are marked with `_`, `__`, `_3`, `_4` and `_5`. Higher octaves are marked with `'`, `''`, `'3` and `'4`. A C natural in octave 2 is therefore `c__ qn`, a B flat in octave 7 is `bf'3 qn`.

Durations are written after the pitch. For notes, the value is the first letter of the duration name (eight, quarter, etc.) followed by `n` for note, e.g., `qn` for quarter note. A dotted duration is given by following the name with a `'`: `hn'` is a dotted half note, with the length of three quarters.

#### Rests

Rests are similar to notes, but with `r` instead of the pitch and an `r` instead of `n` in the duration. A quarter rest is `r qr` and a dotted whole rest is `r wr'`.

#### Chords

Chords are given by their root (a pitch), type, inversion and duration: a C major quarter triad in first inversion is `c maj inv qc`.

* **Root**: specified the same way as in a note (`c`, `af''`, etc.).
* **Type**: three classes of chords: triads, doubled triads and seventh chords.
    * Triads: 3 pitches separated by thirds. 4 types possible: major (`maj`), minor (`min`), augmented (`aug`) and diminished (`dim`), based on the size of the intervals between the pitches.
    * Doubled triads: a triad with the lowermost note doubled an octave above. Same types as the triads, but ending with a `D`, e.g., `majD` or `dimD`.
    * Seventh chords: 4 pitches separated by thirds (the top one is a seventh above the bottom one). 5 types possible: major seventh (`maj7`), major-minor/dominant seventh (`dom7`), minor seventh (`min7`), half-diminished seventh (`hdim7`), diminished seventh (`dim7`).
* **Inversion**: all of the types can be followed by a `'` and then a separate attribute `i0`, `i1`, `i2` or `i3` to specify zeroth, first, second or third inversion: `c maj' i2 qc`. Alternatively, the `inv` attribute can be added (any number of times) to invert a chord once (or any number of times): `c maj inv inv qc`.

Chord durations end with a `c` and can be dotted, as before: `c min7 qc`, `f sharp hdim inv wc'`.

### Composition

Mezzo has two ways of composing music, inspired by [Haskore](https://wiki.haskell.org/Haskore): sequential (melodic) and parallel (harmonic) composition. In addition, Mezzo provides a convenient shorthand input method for melodies.

#### Harmonic composition

Harmonic composition `(:-:)` plays two pieces of music at the same time:

```haskell
g qn :-: e qn :-: c qn
```

For consistency, pieces should be composed from top voice to the bottom: the above example would therefore play a C major triad. The composed pieces can be any musical composition, as long as the durations of the pieces matches. If this is not the case, the shorter voice has to be padded by rests where necessary. A common use case for harmonic composition is playing the right and left hand of a piece at the same time:

```haskell
comp = right :-: left
```

#### Melodic composition

Melodic composition `(:|:)` plays one piece of music after the other:

```haskell
c qn :|: d qn :|: e qn
```

The pieces don't have to only be notes or single voices, but the number of voices in the two pieces must match. For example, the code

```haskell
c qn :|: c maj qc
```

fails to compile (and produces a very cryptic error message), as the first note is only one voice while the chord is three voices. We can remedy this either by explicitly adding rests, or padding the first piece with silent voices, using the functions `pad`, `pad2`, `pad3` or `pad4`:

```haskell
pad2 (c qn) :|: c maj qc
```

This adds empty voices below the existing voices, but in some cases (e.g., a contrapuntal composition), we might want to keep the upper and lower voice and keep the middle voice silent. In this case, we can use the `restWhile` function to input a voice of the same length as the argument, but with no notes.

```haskell
comp = (top :-: restWhile top :-: bottom) :|: c maj qc
```

This example also shows how melodic and harmonic composition works together – as these are just combinators of `Music` values, there is no restriction on the order or nesting of the operators.

### Melodies

Even with builder style, inputting a long melody (sequence of notes and rests) is long and repetitive: the main issue is that *duration change* in melodies is not very frequent, yet we still specify the duration for each note:

```haskell
c qn :|: c en :|: d en :|: ef en :|: d en :|: c en :|: b_ en :|: c hn :|: c hn
```

Mezzo provides a more concise way of melody input, where only the duration changes are explicit:

```haskell
play $ melody :| c :< c :| d :| ef :| d :| c :| b_ :> c :| c
```

Melodies are effectively lists of pitches with the constructors specifying the duration of the next pitch. All melodies have to start with the `melody` keyword – which initialises the melody and set the "default" duration to a quarter note – and a melody can be converted into a playable `Music` value with the `play` function. The constructors can be used as follows:

* `(:|)`: the next note has the same duration as the previous one. For example, `melody :| c :| d :| e` creates a melody of 3 quarter notes (since `melody` initialises the duration to a quarter note).
* `(:<<<)`, `(:<<)`, `(:<)`, `(:^)`, `(:>)` and `(:>>)`: the next note is a thirty-second, sixteenth, eighth, quarter, half or whole note, respectively.
* `(:~|)`: the next rest has the same duration as the previous value (note or rest).
* `(:~<<<)`, `(:~<<)`, `(:~<)`, `(:~^)`, `(:~>)` and `(:~>>)`: as above, but must be followed by a rest.
* All constructors that change the duration (except `(:<<<)` and `(:~<<<)`) can be followed by a `.` to make the duration dotted. For example, `melody :^ c :^. d :> e` specifies a melody of a quarter note, a dotted quarter note and a half note.

#### Examples:

* Frère Jacques:

```haskell
fj1 = play $ melody :| g :| a :| b :| g
fj2 = play $ melody :| b :| c' :> d'
fj3 = play $ melody :< d' :| e' :| d' :| c' :^ b :| g
fj4 = play $ melody :| g :| d :> g

fj = fj1 :|: fj1 :|: fj2 :|: fj2 :|: fj3 :|: fj3 :|: fj4 :|: fj4
```

* Jingle Bells:

```haskell
p1 = play $ melody :< e :| e :^ e :< e :| e :^ e :< e :| g :<. c :<< d :>> e
p2 = play $ melody :< f :| f :<. f :<< f :< f :| e :<. e :<< e :< e :| d :| d :| e :^ d :| g

jb = p1 :| p2
```

## Composition rules

Mezzo enforces a number of composition rules found in classical (common practice) music theory. These can be expanded in the future.

### Harmonic intervals

Mezzo disallows very dissonant harmonic intervals like minor seconds, major sevenths and augmented octaves. That is, `c qn :-: b qn` produces a type error. Note that the rule only applies at harmonic composition and not enforced for chords: that is, a major seventh chord (which contains a major seventh) *is* allowed, but only when constructed symbolically. The composition `c maj7 qc :-: d' qn` is allowed, but `c maj qc :-: b qn :-: d' qn` triggers the error.

### Melodic intervals

Mezzo disallows augmented and diminished melodic intervals, as well as seventh leaps: these are difficult to sing (a common reason for musical constraints) and don't sound good to the ear. Note that due to enharmonic equivalence, an interval might have several names, and some of these may be forbidden. For example, a distance of 3 semitones can be called a minor third (allowed) or an augmented second (disallowed). The reasoning behind this is that minor intervals often appear in music in minor mode, but an augmented interval is usually a given with an accidental and is therefore more "out of place".

A caveat is that Mezzo internally only has three accidentals so it interprets `a flat flat qn` as a G natural. This means that the interval `c qn :|: a flat flat qn` is allowed, even though it is technically a diminished sixth interval.

### Harmonic motion

Mezzo enforces the common rules of harmonic motion, that is, direct motion into perfect intervals (unison, fifth, octave). Two voices singing in parallel a perfect interval apart are very difficult to distinguish: the intervals are so consonant that the voices effectively fuse together, and this does not make for interesting music.

This rule only applies and is only enforced when creating contrapuntal compositions, i.e., when composing two or more melodies in parallel. This means that simple chord progressions with parallel octaves *are* allowed, since the independence of voices is not important in a sequence of chords.

### Other constraints

Mezzo "implicitly" enforces other rules, but these are only required to make the internal enforcement system work.
* As discussed above, concatenation of pieces of music requires both to have the same vertical or horizontal dimensions: this can be achieved by padding the shorter one with voices or rests.
* The shortest possible duration is a thirty-second: this means that dotted thirty-second notes are not allowed.
* The octave limits are -1 to 8 inclusive: the lowest pitch is `c_5`, the highest pitch is `b'4`. For example, `b'4 maj qc` does not type-check, as the third and fifth are out of bounds.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* [Michael B. Gale](http://www.github.com/mbg) for constant support, good ideas and encouragement.
* [Richard Eisenberg](http://www.github.com/goldfirere) for always knowing what to do when I got hopelessly stuck (and also making this project possible).
