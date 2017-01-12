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
