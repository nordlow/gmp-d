# gmp-d
D-language high-level wrapper for GNU MP (GMP) library that aims to

- be `std.bigint.BigInt` compatible

- be `@safe pure nothrow @nogc` when possible

Implementation is highly optimized through

- reimplementation C macro logic in D inline functions for maximum performance

- passing of struct arguments as `auto ref const` for maximum ease and performance
