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

comp = play $ Melody :| c :| d :| e :| f :>> g

main :: IO ()
main = renderMusic "comp.mid" comp
```

Save, build and execute (e.g., with `stack exec <project_name>`). You should get a `.mid` file in the project directory which looks something like this:

![First composition](/docs/comp1.png)

To test the correctness checking, change the `d` in `comp` to a `b`. You should see the following when you save the file:

![First error](/docs/err1.png)

