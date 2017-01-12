# gmp-d
D-language high-level wrapper for GNU MP (GMP) library that aims to

- be `std.bigint.BigInt` compatible
- be `@safe pure nothrow @nogc` when possible
- take struct arguments as `auto ref const` for maximum ease and performance
