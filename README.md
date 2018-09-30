# bigint

Elm's native integer type uses raw JavaScript integers which are limited in size ([MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER)). Sometimes, we want more.

This package provides a `BigInt` type and associated functions so that you can work with integers of unlimited size at the cost of some speed. Benchmarks included.

## Contributions
are very welcome!

## Aknowledgements

- Thank you [Javier Casas](https://github.com/javcasas) whose [elm-integer](https://github.com/javcasas/elm-integer) is the basis for this fork.
- Thank you [gilbertkennen][https://github.com/gilbertkennen] whose [bigint](https://github.com/gilbertkennen/bigint) is basically the verbatim basis for this repository.
- Thank you [hickscorp][https://github.com/hickscorp] whose [bigint](https://github.com/hickscorp/bigint) for the further work on top of the original BigInt library.
