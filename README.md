# gmp-d

D-language high-level wrapper for [GNU MP (GMP) library](https://gmplib.org/)
that aims to
be [mostly compatible](https://github.com/nordlow/gmp-d/blob/master/src/gmp/z.d#L2030)
with `std.bigint.BigInt` (copy construction excluded) and `@safe pure nothrow
@nogc` except when converting to `string`.

## Basics

- Integers (GMP's `mpz_t`) are wrapped in `gmp.z.MpZ`
- Rationals (GMP's `mpq_t`) are wrapped in `gmp.q.MpQ`

along with most of their common operations implemented either as member
functions, free functions or both.

## Features

Copy construction is disabled (for now) to prevent inadvertent copying. Instead
use `f(move(z))` or `f(z.move)` (from `std.algorithm.mutation`) to pass by move
or `f(z.dup)` to pass by value (via `.dup` member function).

Implementation is optimized through

- mapping of GMP's C macros into D inline functions that operate directly on the
  internal C-representations `__mpz_struct` and `__mpq_struct`,

- passing of `MpZ`-typed parameters as `auto ref const` in combination with
  conditional compilation for r-value `MpZ` passed parameters via
  `__traits(isRef)`. This enables clever reuse of `mpz_t` instances when passed
  to `__gmpz`-functions. For instance, `x + 42.Z` can be lowered to

```D
__gmpz_add(rhs._ptr, this._ptr, rhs._ptr);
return move(rhs);
```

in

```D
opBinary!"+"()(auto ref const MpZ rhs)
```

## Performance

Because, wrapper types don't have to be templatized, compilation is very fast;
DMD compiles `gmp-d` in 0.04 seconds on my machine and test-build in 0.22
seconds.

## Limitations

Note that D's `__traits(isRef)` currently cannot be used to distinguish l-value
from r-value passing of `this` (it should). This severly limits the
possibilities of using
C++-style
[expression templates](https://en.wikipedia.org/wiki/Expression_templates) to
realize lazy evaluation in operator overloading. If this limitation were to be
fixed, this library could implement lowering of expressions such

```D
Z result = base^^exp % modulo
```

to the builtin

```D
__gmpz_powm(result._ptr, base._ptr, expr._ptr, modulo._ptr)
```

See the unittests for `MpzAddExpr`, `MpzMulExpr`, etc for details on how this
currently can be implemented and verified (in `ccc`-version) with free
functions such `add` and `sub`.

## Future

There are more `mpz_t` functions that could be wrapped but these are good start.
