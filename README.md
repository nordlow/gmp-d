# gmp-d
D-language high-level wrapper for GNU MP (GMP) library that aims to be
compatible with `std.bigint.BigInt` and be`@safe pure nothrow @nogc`when
possible.

Implementation is highly optimized through

- reimplementation of C macro logic in D inline functions

- passing of struct arguments as `auto ref const`
