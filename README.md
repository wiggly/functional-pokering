# Functional Pokering

[![Build Status](https://travis-ci.org/wiggly/functional-pokering.svg?branch=master)](https://travis-ci.org/wiggly/functional-pokering)

Collection of tools for investigating poker hands, odds, equity and other things to teach myself Haskell.

# Usage

```
fp - poker equity calculator

Usage: fp [-s|--seed SEED] [-t|--trials TRIALS] [-d|--dead DEADCARDS]
          [-b|--board BOARDCARDS] [HANDS...]
            Calculate equity for multiple hands

Available options:
  -h,--help                Show this help text
  -s,--seed SEED           PRNG seed to use
  -t,--trials TRIALS       Number of sample boards to run
  -d,--dead DEADCARDS      Cards known to be dead, no longer in the deck
  -b,--board BOARDCARDS    Cards already on the board        
```

The program currently takes the following options;

* trials Number of trials to run, defaults to 100,000 or the number of possible baords, whichever is lower
* dead Dead cards to be removed from stub when geenrating boards.
* board Cards already on the board to simulate the flop/turn already having been dealt.
* seed Random number seed to use. If not supplied it will use a random seed value.

Arguments following options should be hole-cards that are to be compared.

e.g. To compare the hand Ace of Diamonds and Jack of Diamonds to Ace of Clubs and Three of Clubs

```
bash$ fp AdJd Ac3c
```

e.g. To compare the above hands over a sample size of 50K trials with the Eight of Diamonds being dead and the board consisting of the Two, Four and Five of clubs; 

```
bash$ fp --trials 50000 --dead [8d] --board [2c,4c,5c] AdJd Ac3c
```

# Motivation

I am an experienced developer who wants to learn Haskell for fun and profit. I have looked at Haskell before but never had time to really immerse myself in it.

There are projects I might want to use it for commercially and so I need more than Project Euler problems to learn where the bodies are buried in terms of complexity, performance, functionality.

This collection of libraries/tools is in a domain I understand that provides problems in terms of algorithms, IO and optimisation.

# Status

 * Cards exist and have IO functions to present them neatly in the terminal
 * Decks can be created and shuffled
 * Hold Em hands can be created and ranked
 * Hold Em hand shape can be determined
 * Equity calculations for multiple hands on a sample of boards works
 * Parallel calculator also present
 * Comprehensive option parsing
 
# Build

This package uses the Stack build system.

## Normal

Setup, build and install binaries to standard locations.

```
bash$ stack setup
bash$ stack build
bash$ stack install
```

## Profiling

Build with profiling enabled in libraries.

```
bash$ stack setup
bash$ stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"
```

Run with profiling options. These must come after the ```--``` to ensure all options gets passed to you program instead of the ```stack``` executable.

```
bash$ stack exec fp -- --seed 123456 --trials 10000 AdJd Ac3c +RTS -N -p -s -h -i0.1
```

Inspect ```*.prof``` files or convert ```*.hp``` files into graphs using ```hp2ps```;

```
bash$  hp2ps -c fp.hp
```

Will produce a postscript graph of the heap for that run.

