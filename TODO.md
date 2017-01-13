# TODO list (in order of priority)

- Compare my solution with `std.bigint`
- Use https://github.com/andrew-m-h/libgmp/tree/master/source/deimos/gmp instead of my own extern(C) definitions
- Merge my `Integer` struct with https://github.com/andrew-m-h/libgmp/blob/master/source/deimos/gmp/operators.d
- Is Natural/Integer/Rational (N,Z,Q) or Big{Nat,Int,Rat} preferred?
- Find out if `__gmpz_init` function is same zeroing shallow zeroing of struct
`Integer`. If so only default constructor can be activated.
- Is it preferred to `core.stdc.config : c_long, c_ulong` instead of builtin
  `long` and `ulong` like is done? The motivation should be that these alias to
  `int` and `uint` on non-Posix systems. This sounds like a very unportable
  solution.
- Compare implementation with https://github.com/thestinger/rust-gmp
- Forbid usage of
  - gmp_randinit (not thread-safe, obsolete)
  - mpz_random (not thread-safe, obsolete)
  - mpz_random2 (not thread-safe, obsolete)
  - mpf_set_default_prec (not thread-safe)
  - mpf_get_default_prec (not thread-safe)
  - mpf_init (not thread-safe)
  - mpf_inits (not thread-safe, va_list wrapper)
  - mpf_clears (va_list wrapper)
  - mpf_swap (no better than rust's swap)
  - mpf_set_prec_raw (could be exposed with an unsafe function if needed)
  - mpz_inits (va_list wrapper)
  - mpz_clears (va_list wrapper)
  - mpz_swap (no better than rust's swap)
  - mpq_inits (va_list wrapper)
  - mpq_clears (va_list wrapper)
  - mpq_swap (no better than rust's swap)
