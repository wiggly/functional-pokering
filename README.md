# Functional Pokering

Collection of tools for investigating poker hands, odds, equity and other things to teach myself Haskell.

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
bash$ stack exec fp -- 123456 10000 "AdJd" "Ac3c" +RTS -N -p -s -h -i0.1
```

Inspect ```*.prof``` files or convert ```*.hp``` files into graphs using ```hp2ps```;

```
bash$  hp2ps -c fp.hp
```

Will produce a postscript graph of the heap for that run.

# Usage

The program currently takes input from the command line string. Requires a random seed, number of iterations to perform and Hole Cards to evaluate against one another. e.g.;

```
bash$ stack exec fp 123456 5000 AdJd Ac3c
```

Evaluates AhJh vs Ac3c over 5,000 runs using the random seed 123456

