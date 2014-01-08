# Tybee [![Build Status][1]][2]

Tybee is an asset trading library which uses stochastic nonlinear optimization techniques. Its design and use is detailed below. It is mostly designed and implemented it as a thought experiment. I do not guarentee that this algorithm or this software will be successful or profitable with any asset. It is to be used at ones own risk (see `LICENSE`). It is written in Haskell.

_Status_: infancy

#### Why is it called Tybee?

Tybee is an island off the coast of Georgia. If you go there, make sure you hit up the Crab Shack. Its awesome.

## Documentation

Still working on this...

## Design

Tybee uses a simple idea. Given an asset's history, lets figure out a modifier on the current average price X such that we will buy when the ask price reaches `Cur * X`, then sell when the bid price reaches `Cur * Y` (`Y > X`) or `Cur * Z` (`Z < X`) (a stoploss or floor).

The optimization objective function will simulate this trading behavior with a given set of inputs (X, Y, Z) using historical asset price data. The optimization function is up for discussion, but is currently slated to be [SPSA](http://jhuapl.com/SPSA) ([implementation](https://github.com/yanatan16/haskell-spsa)).

### Getting Started

You can install via `cabal` (or `cabal-dev`)

```
cabal install tybee
```

### Getting the Source

Get the `tybee` source.

    git clone git://github.com/yanatan16/tybee

Set up a sandbox.

The first time through, you need to download and install a ton of
dependencies, so hang in there.

    cd tybee
    cabal-dev install \
        --enable-tests \
        --enable-benchmarks \
        --only-dependencies \
        -j

The `cabal-dev` command is just a sandboxing wrapper around the
`cabal` command.  The `-j` flag above tells `cabal` to use all of your
CPUs, so even the initial build shouldn't take more than a few
minutes.

```
cabal-dev configure --enable-tests --enable-benchmarks
cabal-dev build
```

_Note_: For the development mode use `--flags=developer`. Development mode allows you to run tests from ghci easily to allow some good ole TDD.

### Running Tests

Once you've built the code, you can run the entire test suite in a few
seconds.

```
dist/build/tests/tests +RTS -N
```

We use the direct executable rather than `cabal-dev tests` because it doesn't pass through options very well. The `+RTS -N` above tells GHC's runtime system to use all available cores. If you want to explore, the `tests` program (`dist/build/tests/tests`) accepts a `--help` option. Try it out.


#### Tests From GHCI

You can run all the tests from ghci.

```
cabal-dev ghci
```

starts up the REPL.

```
> import Test.Tybee
> runAllTests
```

Or you can run a single test

```
> runTest "Trade*" -- follows test patterns in Test.Framework
> runGroup 4 -- if you know the group number
```

### Running benchmarks

You can run benchmarks similar to tests.

```
dist/build/benchmarks/benchmarks
```

Just like with tests, there's a `--help` option to explore.

## License

The MIT License found in the LICENSE file.

[1]: https://travis-ci.org/yanatan16/tybee.png?branch=master
[2]: http://travis-ci.org/yanatan16/tybee