# gmp-d

## Overview

D-language high-level wrapper for [GNU MP (GMP) library](https://gmplib.org/)
that aims to be compatible with `std.bigint.BigInt` and be`@safe pure nothrow
@nogc`when possible.

Implementation is optimized through

- mapping of GMP's C macros into D inline functions that operate directly on the
  internal C-representation `__mpz_struct`,

- passing of `MpZ`-typed parameters as `auto ref const`

## TODO list (in order of priority)

- Compare my solution with `std.bigint`

  - Delayed evaluation via expression templates (`Eval.delayed`) is
  planned. Evaluation can kick in automatically for r-value parameters (when
  `!__traits(isRef, param)` when param is passed as `(T)(auto ref const T
  param)`). See
  [this thread](http://forum.dlang.org/post/boorcxnmtatrncrclimp@forum.dlang.org).

  - Copy-on-write (COW) `RefCounted` data store. Optionally with specialized
    heap-allocator for `MpZ` type (16-bytes).

- Use https://github.com/andrew-m-h/libgmp/tree/master/source/deimos/gmp instead of my own extern(C) definitions

- Merge my `Integer` struct with https://github.com/andrew-m-h/libgmp/blob/master/source/deimos/gmp/operators.d

- Define `mp{z,q,f}` C-function wrappers callable with refs to D-wrapper structs
  (`MpZ`, `MpQ`, `MpF`).

- Is for
  - `MpZ`, `MpQ`, `MpF`,
  - `Natural`/`Integer`/`Rational`,
  - (`N`,`Z`,``Q`,`F``) or
  - Big{Nat,Int,Rat,Flt,Float} preferred?

- Find out if `__gmpz_init` function is same zeroing shallow zeroing of struct
`Integer`. If so only default constructor can be activated.

- Is it preferred to `core.stdc.config : c_long, c_ulong` instead of builtin
  `long` and `ulong` like is done? The motivation should be that these alias to
  `int` and `uint` on non-Posix systems. This sounds like a very unportable
  solution.

- Compare implementation with official C++ wrapper
  ([gmpxx](/usr/include/gmpxx.h))
  and [Rust wrappers such as rust-gmp](https://github.com/thestinger/rust-gmp)
  to see that no optimization has been overseen

- Forbid usage of
  - `gmp_randinit` (not thread-safe, obsolete)
  - `mpz_random` (not thread-safe, obsolete)
  - `mpz_random2` (not thread-safe, obsolete)
  - `mpf_set_default_prec` (not thread-safe)
  - `mpf_get_default_prec` (not thread-safe)
  - `mpf_init` (not thread-safe)
  - `mpf_inits` (not thread-safe, va_list wrapper)
  - `mpf_clears` (va_list wrapper)
  - `mpf_swap` (no better than D's own `std.algorithm.swap`)
  - `mpf_set_prec_raw` (could be exposed with an unsafe function if needed)
  - `mpz_inits` (va_list wrapper)
  - `mpz_clears` (va_list wrapper)
  - `mpz_swap` (no better than D's own `std.algorithm.swap`)
  - `mpq_inits` (va_list wrapper)
  - `mpq_clears` (va_list wrapper)
  - `mpq_swap` (no better than D's own `std.algorithm.swap`)

- Lazy evaluation
  via [expression templates](https://en.wikipedia.org/wiki/Expression_templates)
  (when `gmp.Evaluation` is `direct`)
  - `x = -x`        => Assign(x, Neg(x)) => `x.negate()` (if compiler doesn't already rewrite this)
  - `x *= -1`       => `mpz_neg(x, x)` => `x.negate()`
  - `x -= 2*x`      => `mpz_neg(x, x)` => `x.negate()`
  - `x = y + z`     => `mpz_add(x, y, z)`
  - `z = a*x + b*y` => `mpz_add(x, y, z)`
  - `x = x + y * z` => `mpz_addmul(x, y, z)`
  - `x = x ^ y % z` => `mpz_powm(x, x, y, z)`
  - lots more...
  - `toString`, `opCast` should probably evaluate and cache result

- Define a tagged union on top of `__mpz_struct` together with a `ucent` minus
  on bit. Similar to the small {array|vector} optimization used in C++
  libraries. If value is <= `2^^(64-1)-1` it fits in the non-heap allocated
  small value.

- [Code generation](http://forum.dlang.org/post/wyduglxwbxmfcgwtczra@forum.dlang.org) via something like

```D
ulong a = 27, b = 84, c = 110, d = 133;
compileGMP!"Integer res = a ^^ 5 + b ^^ 5 + c ^^ 5 + d ^^ 5"();
```

might generate the code


```D
Integer res, r2; // r2 used as a register of sorts (minimise allocation of Integers)
mpz_init(res); mpz_init(r2);

mpz_ui_pow_ui(res, a, 5);
mpz_ui_pow_ui(r2, b, 5);
mpz_add(res, res, r2);

mpz_ui_pow_ui(res, c 5);
mpz_add(res, res, r2);

mpz_ui_pow_ui(r2, d, 5);
mpz_add(res, res, r2);
```
