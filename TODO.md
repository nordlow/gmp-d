# TODO list (in order of priority)

- Change `scope` to `scope return` or `return scope` for Z parameters whose
  pointers might be copied to a field in the return Z.

- Investigate why `"-checkaction=context"` together with `"-preview=dip1000"` in
  `dflags` of dub buildType unittest fails in `_d_assert_fail` by adding calls
  to

```d
version(none)
private template _d_assert_fail_copy(A...)
{
    string _d_assert_fail(B...)(
        const scope string comp, auto ref const scope A a, auto ref const scope B b) @safe
    if (B.length != 0 || A.length != 1) // Resolve ambiguity with unary overload
    {
        // Prevent InvalidMemoryOperationError when triggered from a finalizer
        if (inFinalizer())
            return "Assertion failed (rich formatting is disabled in finalizers)";

        string[A.length + B.length] vals;
        static foreach (idx; 0 .. A.length)
            vals[idx] = miniFormatFakeAttributes(a[idx]);
        static foreach (idx; 0 .. B.length)
            vals[A.length + idx] = miniFormatFakeAttributes(b[idx]);
        immutable token = invertCompToken(comp);
        return combine(vals[0 .. A.length], token, vals[A.length .. $]);
    }
}
```

- Figure out why statements consisting of a single call to a strongly pure
  function discardig return values, such as `onesComplement(42.Z);`, doesn’t
  trigger a warn-unused.

- Support custom allocation using https://gmplib.org/manual/Custom-Allocation

For instance of `MpZ!(stackSize)` will provide stack-based allocation up to
stackSize before standard allocation via malloc, realloc, free kicks in.

- Support right shift operators `>>` and `>>>`.

- _ccc Make _ccc two => uint _initCCC and uint _opCCC, mutatingCallCount return
  their sum. Use these counters to make more precise asserts on how assignments
  and operators are lowered to calls to C __gmpz-calls

- Disallow construction and assignment from floating point? Check with other GMP
  interfaces and std.bigint.

- Use https://github.com/andrew-m-h/libgmp/tree/master/source/deimos/gmp instead of my own extern(C) definitions

- Define `mp{z,q,f}` C-function wrappers callable with refs to D-wrapper structs
  (`MpZ`, `MpQ`, `MpF`).

- Is for
  - `MpZ`, `MpQ`, `MpF`,
  - `Natural`/`Integer`/`Rational`,
  - (`N`,`Z`,``Q`,`F``) or
  - Big{Nat,Int,Rat,Flt,Float} preferred?

- Is it preferred to `core.stdc.config : c_long, c_ulong` instead of builtin
  `long` and `ulong` like is done? The motivation should be that these alias to
  `int` and `uint` on non-Posix systems. This sounds like a very unportable
  solution.

- Compare implementation with official C++ wrapper
  ([gmpxx](/usr/include/gmpxx.h))
  and [Rust wrappers such as rust-gmp](https://crates.io/crates/rust-gmp)
  to see that no optimization has been overseen

- Delayed evaluation via expression templates is in development. Evaluation can
kick in automatically for r-value parameters (`!__traits(isRef, param)` when
param is passed as `(T)(auto ref const T param)`). See [this
thread](http://forum.dlang.org/post/boorcxnmtatrncrclimp@forum.dlang.org). Lazy
evaluation via [expression
templates](https://en.wikipedia.org/wiki/Expression_templates)

  - `x = -x`        => Assign(x, Neg(x)) => `x.negate()` (if compiler doesn't already rewrite this)
  - `x *= -1`       => `mpz_neg(x, x)` => `x.negate()`
  - `x -= 2*x`      => `mpz_neg(x, x)` => `x.negate()`
  - `x = y + z`     => `mpz_add(x, y, z)`
  - `z = a*x + b*y` => `mpz_add(x, y, z)`
  - `x = x + y * z` => `mpz_addmul(x, y, z)` or `mpz_addmul_ui(x, y, z)`
  - `x = x - y * z` => `mpz_submul(x, y, z)` or `mpz_submul_ui(x, y, z)`
  - `x = x ^ y % z` => `mpz_powm(x, x, y, z)`
  - lots more...
  - `toString`, `opCast` should probably evaluate and cache result

- Define a tagged union on top of `__mpz_struct` together with a `ucent` minus
  one discriminator bit. Similar to the small {array|vector} optimization used
  in C++ libraries. If value is <= `2^^(64-1)-1` it fits in the non-heap
  allocated small value.

- Don't use

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

- [Code generation](http://forum.dlang.org/post/wyduglxwbxmfcgwtczra@forum.dlang.org)
