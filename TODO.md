# TODO list (in order of priority)

- Compare my solution with `std.bigint`

- Use https://github.com/andrew-m-h/libgmp/tree/master/source/deimos/gmp instead of my own extern(C) definitions

- Merge my `Integer` struct with https://github.com/andrew-m-h/libgmp/blob/master/source/deimos/gmp/operators.d

- Define mp{z,q,f}-function wrappers for Mp{Z,Q,F} class.

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

- Compare implementation with https://github.com/thestinger/rust-gmp

- Forbid usage of
  - `gmp_randinit` (not thread-safe, obsolete)
  - `mpz_random` (not thread-safe, obsolete)
  - `mpz_random2` (not thread-safe, obsolete)
  - `mpf_set_default_prec` (not thread-safe)
  - `mpf_get_default_prec` (not thread-safe)
  - `mpf_init` (not thread-safe)
  - `mpf_inits` (not thread-safe, va_list wrapper)
  - `mpf_clears` (va_list wrapper)
  - `mpf_swap` (no better than rust's swap)
  - `mpf_set_prec_raw` (could be exposed with an unsafe function if needed)
  - `mpz_inits` (va_list wrapper)
  - `mpz_clears` (va_list wrapper)
  - `mpz_swap` (no better than rust's swap)
  - `mpq_inits` (va_list wrapper)
  - `mpq_clears` (va_list wrapper)
  - `mpq_swap` (no better than rust's swap)

- Lazy evaluation
  via [expression templates](https://en.wikipedia.org/wiki/Expression_templates)
  (when `gmp.Evaluation` is `direct`)
  - `x = -x` => Assign(x, Neg(x)) => `x.negate()` (if compiler doesn't already rewrite this)
  - `x *= -1` => x.negeate()
  - `x = x + y * z` => `mpz_addmul(x, y, z)`
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


```
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
