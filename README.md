# gmp-d
D-language high-level wrapper for GNU MP (GMP) library that aims to be
compatible with `std.bigint.BigInt` and be`@safe pure nothrow @nogc`when
possible.

Implementation is highly optimized through

- mapping of GMP's C macros into D inline functions

- passing of struct arguments as `auto ref const`

Delayed evaluation via expression templates (`Eval.delayed`) is planned.
