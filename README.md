# gmp-d

D-language high-level wrapper for [GNU MP (GMP) library](https://gmplib.org/)
that aims to be compatible with `std.bigint.BigInt` and `@safe pure nothrow
@nogc` except when converting to `string`.

Implementation is optimized through

- mapping of GMP's C macros into D inline functions that operate directly on the
  internal C-representation `__mpz_struct`,

- passing of `MpZ`-typed parameters as `auto ref const`. This enables clever
  reuse of `mpz_t` instances in when passing them to `__gmpz`-functions.

Copy construction is currently disabled for now. Instead use `move(z)` to pass
by move or `z.dup` property if duplication is needed.

There are more `mpz_t` functions that could be wrapped but these are good start.
