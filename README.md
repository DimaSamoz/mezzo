# Mezzo

[![Build Status](https://travis-ci.org/DimaSamoz/mezzo.svg?branch=master)](https://travis-ci.org/DimaSamoz/mezzo) [![Hackage](https://img.shields.io/hackage/v/mezzo.svg)](https://hackage.haskell.org/package/mezzo)

*Mezzo* is a Haskell library and embedded domain-specific language for music description. Its novelty is in the fact that it can enforce various rules of music composition *statically*, that is, at compile-time. This effectively means that if you write "bad" music, your composition will not compile – think of it as a **very** strict spell-checker for music.

- [Getting started](#getting-started)
	- [Prerequisites](#prerequisites)
	- [Installation](#installation)
	- [First composition](#first-composition)
- [Composing in Mezzo](#composing-in-mezzo)
	- [Basic concepts](#basic-concepts)
	- [Literal vs. builder style](#literal-vs-builder-style)
	- [Primitives](#primitives)
		- [Notes](#notes)
		- [Rests](#rests)
		- [Chords](#chords)
	- [Composition](#composition)
		- [Harmonic composition](#harmonic-composition)
		- [Melodic composition](#melodic-composition)
	- [Melodies](#melodies)
		- [Examples:](#examples)
	- [Chord progressions](#chord-progressions)
		- [Harmonic regions](#harmonic-regions)
		- [Phrases](#phrases)
- [Scores](#scores)
	- [Score building](#score-building)
- [Rendering and exporting](#rendering-and-exporting)
- [Rule sets](#rule-sets)
	- [The `free` rule set](#the-free-rule-set)
	- [The `classical` rule set](#the-classical-rule-set)
	- [The `strict` rule set](#the-strict-rule-set)
	- [Custom rule sets](#custom-rule-sets)
	- [Other constraints](#other-constraints)
- [License](#license)
- [Acknowledgments](#acknowledgments)

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
extra-deps: [ mezzo-0.3.0.0 ]

build-depends: base >= 4.7 && < 5
             , mezzo
```

Build the file, and you should be good to go.


### First composition

Create a new project (e.g. with `stack new`) with a `Main` module. Type:

```haskell
import Mezzo

comp = defScore $ play $ melody :| c :| d :| e :| f :>> g

main :: IO ()
main = renderScore "comp.mid" "First composition" comp
```

Save, build and execute (e.g. with `stack exec <project_name>`). You should get a `.mid` file in the project directory which looks something like this:

![First composition](https://raw.githubusercontent.com/DimaSamoz/mezzo/master/docs/comp1.png)

To test the correctness checking, change the `d` note in `comp` to a `b`. You should see the following when you save the file:

```
• Major sevenths are not permitted in melody: C and B
• In the first argument of ‘(:|)’, namely ‘melody :| c :| b’
  In the first argument of ‘(:|)’, namely ‘melody :| c :| b :| e’
  In the first argument of ‘(:>>)’, namely
    ‘melody :| c :| b :| e :| f_’
```

## Composing in Mezzo

This section provides more detail on the syntax of Mezzo.

### Basic concepts

*Music description languages* are textual representations of musical pieces, used for note input or transcription. Most MDLs provide ways of inputting notes, rests and ways to combine these into melodies or chords. Mezzo additionally lets users input *chords* in their symbolic representation, as well as *chord progressions* using a schematic description of functional harmony.

### Literal vs. builder style

Mezzo provides two main ways of creating musical values:

* **Literal style**: create musical values with explicit constructors of literal values. For example a C natural in octave 4 is written as `pitch _c _na _o4` and can be abbreviated to `_cn`. A quarter note (pitch with duration) is then written as `noteP (pitch _c _na _o4) _qu` or `noteP _cn _qu`. Literal values are prefixed with an underscore and can be combined using the constructors `pitch` and `noteP`.
* **Builder style**: create musical values by sequencing their attributes from left to right. This style is more concise, flexible and readable than literal style and is therefore preferred. Creating a middle C quarter note is `c qn`, an F sharp dotted sixteenth note in the 5th octave is `fs sn'` or `f sharp sn'`. Similarly, a C major chord would be `c maj qc`, a B flat diminished seventh in first inversion with dotted half duration would be `b flat dim7 inv hc'`.

The rest of this guide uses the builder style.

### Primitives

#### Notes

Notes are given by their pitch and duration. In builder style, every pitch has an explicit value, consisting of three parts:

* **Pitch class**: one of `c`, `d`, `e`, `f`, `g`, `a` or `b`; determines the position of the note in the octave, a white key on the keyboard.
* **Accidental**: a suffix of the pitch class, one of `f` (flat, e.g. `bf qn`) or `s` (sharp, e.g. `fs qn`). Natural accidentals are not specified, so `c` means C natural. Accidentals can also be written out as a separate attribute (`c sharp qn`), or even repeated (for example, double sharps: `c sharp sharp qn` or `cs sharp qn`).
* **Octave**: the last component of the value. The default octave is 4, this is unmarked. Lower octaves are marked with `_`, `__`, `_3`, `_4` and `_5`. Higher octaves are marked with `'`, `''`, `'3` and `'4`. A C natural in octave 2 is therefore `c__ qn`, a B flat in octave 7 is `bf'3 qn`.

Durations are written after the pitch. For notes, the value is the first letter of the duration name (eighth, quarter, etc.) followed by `n` for note, e.g., `qn` for quarter note. A dotted duration is specified by following the name with a `'`: `hn'` is a dotted half note, with the length of three quarters.

#### Rests

Rests are similar to notes, but with `r` instead of the pitch and an `r` instead of `n` in the duration. A quarter rest is `r qr` and a dotted whole rest is `r wr'`.

#### Chords

Chords are given by their root (a pitch), type, inversion and duration: a C major quarter triad in first inversion is `c maj inv qc`.

* **Root**: specified the same way as in a note (`c`, `af''`, etc.).
* **Type**: three classes of chords: dyads, triads and tetrads.
    * Dyads: A harmonic interval consisting of 2 notes. 5 types possible: minor thirds (`min3`), major thirds (`maj3`), fourths (`fourth`), fifths (`fifth`) and octaves (`oct`).
    * Triads: 3 pitches separated by thirds. 4 types possible: major (`maj`), minor (`min`), augmented (`aug`) and diminished (`dim`), based on the size of the intervals between the pitches.
    * Tetrads: 4 pitches separated by thirds (the top one is a seventh above the bottom one). 5 types possible: major seventh (`maj7`), major-minor/dominant seventh (`dom7`), minor seventh (`min7`), half-diminished seventh (`hdim7`), diminished seventh (`dim7`).
* **Doubling**: dyads and triads can be doubled to become a triad and tetrad respectively. This is done by repeating the bottommost note an octave higher, so a doubled C major third would consist of a C, an E, and another C an octave above. Doubling is specified by a `D` after the chord type, e.g. `fifthD`, `majD` or `augD`.
* **Inversion**: all of the types can be followed by a `'` and then a separate attribute `i0`, `i1`, `i2` or `i3` to specify zeroth, first, second or third inversion: `c maj' i2 qc`. Alternatively, the `inv` attribute can be added (any number of times) to invert a chord once (or any number of times): `c maj inv inv qc`.

Chord durations end with a `c` and can be dotted, as before: `c min7 qc`, `f sharp hdim inv wc'`.

### Composition

Mezzo has two ways of composing music, inspired by [Haskore](https://wiki.haskell.org/Haskore): sequential (melodic) and parallel (harmonic) composition. In addition, Mezzo provides a convenient shorthand input method for melodies.

#### Harmonic composition

Harmonic composition `(:-:)` plays two pieces of music at the same time:

```haskell
g qn :-: e qn :-: c qn
```

For consistency, pieces should be composed from top voice to the bottom: the above example would therefore play a C major triad. The composed pieces can be any musical composition, as long as the durations of the pieces matches. If this is not the case, the shorter voice has to be padded by rests where necessary.

When using `(:-:)`, both harmonic intervals and harmonic motion are checked for correctness, so it is mainly intended for contrapuntal compositions. For homophonic compositions (where the "voices" are not independent, the top one being the main melody and the bottom ones being the accompaniment), you can use the `hom` function which does not enforce rules of harmonic motion. For example,

```haskell
(g qn :|: a qn) :-: (c qn :|: d qn)
```

would not compile due to a parallel fifth (`(:|:)` is melodic composition, described in the next section). However,

```haskell
(g qn :|: a qn) `hom` (c qn :|: d qn)
```

compiles, as `hom` only checks for harmonic dissonance.

#### Melodic composition

Melodic composition `(:|:)` plays one piece of music after the other:

```haskell
c qn :|: d qn :|: e qn
```

The pieces don't have to only be notes or single voices, but the number of voices in the two pieces must match. For example, the code

```haskell
c qn :|: c maj qc
```

fails to compile (and unfortunately produces a very cryptic error message), as the first note is only one voice while the chord is three voices. We can remedy this either by explicitly adding rests, or padding the first piece with silent voices, using the functions `pad`, `pad2`, `pad3` or `pad4`:

```haskell
pad2 (c qn) :|: c maj qc
```

This adds empty voices below the existing voices, but in some cases (e.g. a contrapuntal composition), we might want to keep the upper and lower voice and keep the middle voice silent. In this case, we can use the `restWhile` function to input a voice of the same length as the argument, but with no notes.

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

Below is a table summarising the melody construction operators.

| Duration      | Note  | Rest  | Dotted note | Dotted rest |
|---------------|-------|-------|-------------|-------------|
| No change     | <code>:&#124;</code> | <code>:~&#124;</code> |  | |
| Thirty-second | `:<<<`  | `:~<<<` |               |               |
| Sixteenth     | `:<< `  | `:~<<`  | `:<<.`        | `:~<<.`       |
| Eighth        | `:<`    | `:~<`   | `:<.`         | `:~<.`        |
| Quarter       | `:^`    | `:~^`   | `:^.`         | `:~^.`        |
| Half          | `:>`    | `:~>`   | `:>.`         | `:~>.`        |
| Whole         | `:>>`   | `:~>>`  | `:>>.`        | `:~>>.`       |


#### Examples

* Frère Jacques:

```haskell
fj1 = play $ melody :| g :| a :| b :| g
fj2 = play $ melody :| b :| c' :> d'
fj3 = play $ melody :< d' :| e' :| d' :| c' :^ b :| g
fj4 = play $ melody :| g :| d :> g

fj = defScore $ fj1 :|: fj1 :|: fj2 :|: fj2 :|: fj3 :|: fj3 :|: fj4 :|: fj4
```

* Jingle Bells:

```haskell
p1 = play $ melody :< e :| e :^ e :< e :| e :^ e :< e :| g :<. c :<< d :>> e
p2 = play $ melody :< f :| f :<. f :<< f :< f :| e :<. e :<< e :< e :| d :| d :| e :^ d :| g

jb = defScore $ p1 :|: p2
```

### Chord progressions

Mezzo implements a subset of the [functional harmony grammar](http://www.tandfonline.com/doi/abs/10.1080/17459737.2011.573676) of Martin Rohrmeier and is inspired by [HarmTrace](https://hackage.haskell.org/package/HarmTrace). The idea is that chord progressions are described schematically using *functional harmony*, a harmonic system which assigns various roles or *functions* to different chords of a key. For example, the chord of the first degree (called the *tonic*) represents stability and "home", while the fifth degree chord (the *dominant*) creates harmonic tension which has to be resolved into the tonic. The Mezzo EDSL for creating progressions guarantees that harmonic functions are handled and constructed correctly. For example, the following is a I–IV–V–I–IV–V7–I progression, consisting of one tonic-dominant-tonic phrase and a cadence:

```haskell
p = prog $ ph_IVI ton dom_V ton :+ cadence (full subdom_IV auth_V7_I)
```

#### Harmonic regions

*Harmonic regions* are one of more chords of a certain harmonic function. For example, a tonic region can consist of one or more tonic chords. The actual nature of the chords (e.g. key, mode, etc.) is determined by the key signature of the piece, described in the [next section](#scores). The regions are either single values (e.g. `dom_ii_V7` generates a secondary dominant chord followed by a dominant chord) or combinators of regions (e.g. `dom_S_D` creates a dominant region from a subdominant region and a dominant region). The chords generated are sequences of quarter chords in the third octave, with appropriate inversions so the progression sounds as conjunct as possible.

* Tonic regions
    * `ton`: one tonic chord.
    * `ton_T_T t1 t2`: two consecutive tonic regions.
* Dominant regions:
    * `dom_V`: a major fifth degree chord.
    * `dom_V7`: a dominant fifth degree chord.
    * `dom_vii0`: a diminished seventh degree chord.
    * `dom_II_V7`: a secondary dominant (second degree) followed by a dominant (fifth degree) chord.
    * `dom_S_D sd d`: a subdominant region followed by a dominant region.
    * `dom_D_D d1 d2`: two consecutive dominant regions.
* Subdominant regions:
    * `subdom_IV`: a fourth degree chord.
    * `subdom_ii`: a second degree minor chord.
    * `subdom_iii_IV`: a third degree minor chord followed by a fourth degree major chord.
    * `subdom_S_S sd1 sd2`: two consecutive subdominant regions.

#### Phrases
A chord progression is broken up into *phrases*, composed using the `:+` operator. The last phrase must always be a *cadential phrase*, which provides closure to the piece. Phrase constructors (such as `ph_VI`) take tonic and dominant regions as arguments, which are described above. The cadential phrase is constructed using the `cadence` keyword, and followed by a cadential region.

* Phrases:
    * `ph_I t`: a tonic phrase, consisting of a single tonic region.
    * `ph_VI d t`: a dominant-tonic phrase, consisting of a dominant and tonic region
    * `ph_IVI t1 d t2`: a tonic-dominant-tonic phrase, consisting of a tonic, dominant and tonic region.
* Cadential regions:
    * `auth_V_I`: an authentic V–I cadence.
    * `auth_V7_I`: an authentic dominant V7–I cadence.
    * `auth_vii_I`: an authentic leading tone vii–I cadence.
    * `auth_64_V7_I`: an authentic dominant cadence with a cadential 6-4.
    * `decept_V_iv`: a deceptive V-iv cadence.
    * `full sd c`: a full cadence, consisting of a subdominant region and a cadence.

The example above therefore has a tonic-dominant-tonic region, followed by a full cadence with a fourth degree subdominant and authentic dominant cadence. The `prog` keyword converts a schema into a `Music` value.

```haskell
p = prog $ ph_IVI ton dom_V ton :+ cadence (full subdom_IV auth_V7_I)
```

## Scores

`Music` values cannot be immediately exported into MIDI files, as there are various attributes of the piece that need to be specified first (in particular, the [rule set](#rule-sets) used to check the correctness of the composition, without which the pieces will not compile). This is accomplished by constructing a `Score` out of a `Music` value, and specifying the attributes of the piece such as the tempo, time signature or rule set. Scores can also be used to break a larger musical piece into multiple sections which are "pre-rendered": this significantly cuts down on rule-checking time, as the correctness of the sections is checked independently. This also allows tempo or key changes, since each section can have its own set of attributes.

### Score building
Scores are built using the following syntax:

```
score [<attribute_name> <attribute_value>]* withMusic <music>
```

For example, the following creates a new score from a `Music` value `m` for the refrain section of a piece in A minor and quadruple (common) meter, with a tempo of 120 BPM and strict rule-checking:

```haskell
comp = score section "refrain"
             setKeySig a_min
             setTimeSig quadruple
             setTempo 120
             setRuleSet strict
             withMusic m
```

* The `score` keyword starts building a score and must be followed by the keyword `withMusic` with zero or more optional attributes in-between.
* The score attributes are simple name-value pairs written one after the other, with no punctuation (line breaks, as above, are optional). Mezzo currently supports the following attributes:

| Attribute name | Attribute type | Default value   | Description                                                                                                                                                                                                            |
|----------------|----------------|-----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `section`      | string         | `"Composition"` | A brief description of the section of the piece that the score describes.  This attribute is only for documentation purposes, to make it clear in the source code which section is where.                              |
| `setKeySig`    | key signature  | `c_maj`         | The key signature of the score, e.g. `c_maj`, `a_min`, `fs_maj`, `bf_min`. This affects the key of the chord progressions.                                                                                             |
| `setTimeSig`   | time signature | `quadruple`     | The time signature of the score, one of `duple`, `triple`, or `quadruple`.  This affects the number of chords played in a bar of a progression  (e.g. three chords for triple meter, four chords for quadruple meter). |
| `setTempo`     | integer        | 120             | The tempo of the piece, in BPM. This affects the tempo of the MIDI file playback.                                                                                                                                      |
| `setRuleSet`   | rule set       | `classical`     | The rule set used to check the correctness of a piece. One of `free`, `classical`, `strict`, or any other user-defined rule set.                                                                                       |

* The `withMusic` keyword finishes building a score from its argument of type `Music`.
* If no attributes are modified (i.e. the default score attributes are used), the shorthand function `defScore m` can be used instead of `score withMusic m` to create a score from the piece `m`.

## Rendering and exporting

Once a score is created from a musical piece, it can be exported as a MIDI file using the `renderScore` function:

```haskell
renderScore :: FilePath -> Title -> Score -> IO ()
```

The function takes a file path (specified as a `String`), the title of the MIDI track (also a `String`) and the score to export. Upon successful rendering, you should see the message `Composition rendered to <filepath>.`.

If a piece is broken up into smaller scores (sections), all of them can be concatenated and rendered using the `renderScores` function:

```haskell
renderScores :: FilePath -> Title -> [Score] -> IO ()
```

This is similar to `renderScore`, but takes a list of `Score`s (in the desired order). Note that this allows for reuse of sections: for example, a refrain (or repeated section) only has to be described once, and the score created from it can be reused without any additional rule-checking. See the Für Elise example for a demonstration.

Mezzo compositions can be played live, from the terminal, using the function

```haskell
playLive' :: Score -> IO ()
```
You can also use `playLive = playLive' . defScore` to play a `Music` value right away. Note that in order to play Mezzo compositions from the terminal, you need to have an appropriate MIDI device (e.g. synthesiser) configured. As the implementation uses Euterpea's playback features, you should set up the configuration following [these](http://www.euterpea.com/euterpea/setting-up-midi/) instructions.

## Rule sets
As different musical genres enforce different kinds of rules, Mezzo lets users select different levels of strictness (called *rule sets*) or even completely customise the musical rules that are checked. In particular, one rule set turns off correctness checking completely, allowing for complete creative freedom.

Mezzo predefines three rule sets of increasing strictness. These can be specified using the `setRuleSet` score attribute, as described in [Score building](#score-building).

### The `free` rule set
No musical rules enforced, the music is free from restrictions.

### The `classical` rule set
Enforces the common rules of classical music without being too restrictive. In particular:

* Very dissonant harmonic intervals like minor seconds, major sevenths and augmented octaves are forbidden. For example, `c qn :-: b qn` produces a type error. Note that the rule only applies at harmonic composition and is not enforced for chords: that is, a major seventh chord (which contains a major seventh) *is* allowed, but only when constructed symbolically.
* Large melodic intervals such as major sevenths or compound intervals are forbidden. These break up the flow of a melody, making it sound disjunct.
* Rules of harmonic motion, such as parallel and concealed unisons, fifths and octaves, are forbidden for harmonic composition of voices. Two voices singing in parallel a perfect interval apart are very difficult to distinguish: the intervals are so consonant that the voices effectively fuse together, and this does not make for interesting music. These rules are only enforced for contrapuntal harmonic composition (the `(:-:)` operator) of individual voices, so sequential composition of chords (the `(:|:)` operator) or chord progressions *are* allowed: the independence of voices is not important in a sequence of chords.

### The `strict` rule set
Enforces most of the rules of strict contrapuntal compositions, which are often based around vocal performance. This rule set is the most restrictive and is generally leads to longer compile times, but is probably the most useful for students of composition.

* Dissonant harmonic intervals are disallowed, just as in the `classical` rule set. However, major seventh chords are also disallowed.
* Large melodic intervals (sevenths and compound intervals) are disallowed, and so are augmented and diminished melodic intervals. These are difficult to sing and don't sound good to the ear. Note that due to enharmonic equivalence, an interval might have several names, and some of these may be forbidden. For example, a distance of 3 semitones can be called a minor third (allowed) or an augmented second (disallowed). The reasoning behind this is that minor intervals often appear in music in minor mode, but an augmented interval is usually given with an accidental and is therefore more "out of place".
A caveat is that Mezzo internally only has three accidentals so it interprets `a flat flat qn` as a G natural. This means that the interval `c qn :|: a flat flat qn` is allowed, even though it is technically a diminished sixth interval.
* Harmonic motion constraints are enforced for harmonic *and* melodic composition (`(:-:)` and `(:|:)`), as well as homophonic composition (`hom`). Symbolic chord progressions are still allowed, even if they violate harmonic motion rules – however, motion rules between a progression accompaniment and a melodic line are enforced.

### Custom rule sets
Users of Mezzo can define their own custom rule sets, albeit adding complex rules can require a closer understanding of the Mezzo internals and type-level computation. Rule sets are a collection of constraints (i.e. Haskell type class constraints), one for all of the ways a `Music` value can be constructed: harmonic composition, melodic composition, homophonic composition, rests, notes, chords, progressions.

Any value can be used as a rule set, as long as its type is an instance of the `RuleSet` type class:

```haskell
class RuleSet t where
    type MelConstraints   t (m1 :: Partiture n l1) (m2 :: Partiture n l2) :: Constraint
    type HarmConstraints  t (m1 :: Partiture n1 l) (m2 :: Partiture n2 l) :: Constraint
    type NoteConstraints  t (r :: RootType)        (d :: Duration)        :: Constraint
    type RestConstraints  t                        (d :: Duration)        :: Constraint
    type ChordConstraints t (c :: ChordType n)     (d :: Duration)        :: Constraint
    type ProgConstraints  t (s :: TimeSignature)   (p :: ProgType k l)    :: Constraint
    type HomConstraints   t (m1 :: Partiture n1 l) (m2 :: Partiture n2 l) :: Constraint
```

The various constraints take as input the arguments of the corresponding `Music` constructor: for example, `HarmConstraints` takes the two type-level composed musical pieces (of type `Partiture`) as arguments, and returns a `Constraint` (see ConstraintKinds extension) expressing whether the pieces can be harmonically composed.

Custom rule sets can be useful even if the exact details of Mezzo's type-level model are not understood. For example, suppose that we want to create a very restrictive rule set for first-species counterpoint: this species only allows for melodic lines containing whole notes and enforces most of the classic rules of harmony, melody and motion. To implement this rule set, we can restrict the constructors that are allowed and delegate the complex rule-checking to the existing Mezzo implementation. To start, we define a new type for our rule set:

```haskell
data FirstSpecies = FirstSpecies
```

Now, we make `FirstSpecies` an instance of the `RuleSet` type class by defining the associated constraint synonyms. In this case, we want to:

* Use the same musical rule-checking methods as the `strict` rule set
* Only allow whole note and rest durations
* Disallow chords and progressions.

To achieve this, we declare the following instance of the `RuleSet` class for `FirstSpecies`:

```haskell
instance RuleSet FirstSpecies where
    type MelConstraints   FirstSpecies m1 m2 = MelConstraints Strict m1 m2
    type HarmConstraints  FirstSpecies m1 m2 = HarmConstraints Strict m1 m2
    type HomConstraints   FirstSpecies m1 m2 = HomConstraints Strict m1 m2
    type NoteConstraints  FirstSpecies r d   = ValidDur d
    type RestConstraints  FirstSpecies d     = ValidDur d
    type ChordConstraints FirstSpecies c d   = InvalidChord
    type ProgConstraints  FirstSpecies t p   = InvalidProg
```

* `MelConstraints`, `HarmConstraints` and `HomConstraints` simply delegate rule-checking to the constraints of the `Strict` rule set.
* `NoteConstraints` and `RestConstraints` are defined using the `ValidDur` class, defined as follows:
```haskell
class ValidDur (d :: Duration)
instance ValidDur Whole
instance {-# OVERLAPPABLE #-}
        TypeError (Text "First species counterpoint only have whole durations.")
        => ValidDur d
```
  This makes use of GHC's [custom type errors](https://ghc.haskell.org/trac/ghc/wiki/Proposal/CustomTypeErrors) feature, with the correspoinding types available in the GHC.TypeLits module. We define the `ValidDur` type class (restricting its argument to a type of kind `Duration`) without any methods. The `Whole` duration is made an instance of `ValidDur`, expressing our intention to make whole durations valid. The overlappable instance for `ValidDur d` is selected only if the previous instance does not fit (i.e. the duration is not `Whole`): in this case, a custom type error is encountered and type-checking fails, displaying our custom error message.
* `ChordConstraints` and `ProgConstraints` are simply equal to the "constant" (nullary) type classes `InvalidChord` and `InvalidProg`, which are defined in a similar manner to `ValidDur`:

```haskell
class InvalidChord
instance TypeError (Text "Chords are not allowed in counterpoint.")
         => InvalidChord

class InvalidProg
instance TypeError (Text "Progressions are not allowed in counterpoint.")
         => InvalidProg
```
  In this case, the constraints always fail (since they have no arguments), displaying the corresponding error message.

Having defined this instance, we can expect to see our custom constraints any time we use the `FirstSpecies` rule set. For example, the following composition fails to compile:

```haskell
comp = score setRuleSet FirstSpecies
             withMusic (c maj qc :-: b qn)
```

The type error messages explain what the problem is, just as we defined them:

```
• Can't have major sevenths in chords: C and B
• Chords are not allowed in counterpoint.
• First species counterpoint only have whole durations.
```

The full file (with the necessary language extensions and module imports needed) can be found in the [examples folder](https://github.com/DimaSamoz/mezzo/blob/master/examples/src/Other/FirstSpecies.hs).

### Other constraints

Mezzo "implicitly" enforces other rules, but these are only required to make the internal enforcement system work. These rules are enforced independently of the rule set selected.

* As discussed above, concatenation of pieces of music requires both to have the same vertical or horizontal dimensions: this can be achieved by padding the shorter one with voices or rests.
* The shortest possible duration is a thirty-second: this means that dotted thirty-second notes are not allowed.
* Progressions generally follow the key of the score, but some harmonic regions are only valid in one of the two modes. In particular, the subdominant regions `subdom_ii` `subdom_iii_IV` can only appear in a major mode piece.
* The octave limits are -1 to 8 inclusive: the lowest pitch is `c_5`, the highest pitch is `b'4`. For example, `b'4 maj qc` does not type-check, as the third and fifth are out of bounds.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

Note that this project is different from [Mezzo](http://protz.github.io/mezzo/), a research language for effectful concurrent programming.

## Acknowledgments

* [Michael B. Gale](http://www.github.com/mbg) for constant support, good ideas and encouragement.
* [Richard Eisenberg](http://www.github.com/goldfirere) for always knowing what to do when I got hopelessly stuck using TypeInType...
* [Simon Peyton Jones](https://github.com/simonpj) for a stimulating discussion about the project and valuable advice on future work.
