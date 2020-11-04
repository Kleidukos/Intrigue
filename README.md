# [Intrigue][] [![GitHub Actions][Github Actions]](https://github.com/Kleidukos/Intrigue/actions) ![Simple Haskell][Simple Haskell]

This is my implementation of a R⁵RS Scheme interpreter.  
Very unsuitable for production.

It also serves as a playground to try out:
* Megaparsec (instead of the venerable Parsec) for lexing and parsing
* MTL for holding state and general program architecture

## Build and Run

Run `$ stack build` or `cabal new-build` in order to build the project

Run `$ stack test` or `cabal new-test` in order to run the test suite

## Acknowledgement

I wish to give my thanks to

* Koz Ross
* Mark Karpov

[Intrigue]: https://github.com/kleidukos/Intrigue
[Simple Haskell]: https://www.simplehaskell.org/badges/badge2.svg
[GitHub Actions]: https://github.com/Kleidukos/Intrigue/workflows/CI/badge.svg
