# Phoskell

An image processing library in Haskell.

## Usage

This project comes with an executable application, which you can run with the following cabal command in terminal:

```sh
cabal run phoskell -- <path/to/image>
```

The code for this application is held in the file `app/Main.hs`, so changing the code in that file will be reflected when running the command above.

## Evaluating

The library has also been tested and benchmarked, both of which can be repeated fairly easily.

### Testing

To test the library, simply run:

```sh
cabal test
```

### Benchmarking

For benchmarking, as there are several benchmarking suites included in this project, there are a few options to choose from:

```sh
cabal bench point
cabal bench histogram
cabal bench convolution
cabal bench synthesis
```

Alternatively, to run all benchmarks, run:

```sh
cabal bench
```
