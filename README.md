# gmp-d

D-language high-level wrapper for [GNU MP (GMP) library](https://gmplib.org/)
that aims to be [mostly
compatible](https://github.com/nordlow/gmp-d/blob/master/src/gmp/z.d#L2030) with
`std.bigint.BigInt` and `@safe pure nothrow @nogc` except when converting to
`string`.

## Basics

- Integers (GMP's `mpz_t`) are wrapped in `gmp.z.MpZ`
- Rationals (GMP's `mpq_t`) are wrapped in `gmp.q.MpQ`

along with most of their common operations implemented either as member
functions, free functions or both.

## Sample Usage

### MpZ

```D
import gmp.z;

@safe pure nothrow @nogc:

unittest
{
    alias Z = MpZ;
    assert(Z.mersennePrime(15) == 2^^15 - 1);
    const a = 42.Z;
    const b = a.dup; // explicit copying required
}
```

### MpQ

```D
import gmp.q;

@safe pure nothrow @nogc:

unittest
{
    alias Q = MpQ;

    Q x = Q(-4, 6);

    assert(x.numerator == -4);
    assert(x.denominator == 6);

    x.canonicalize();

    assert(x.numerator == -2);
    assert(x.denominator == 3);

    assert(inverse(x) == Q(-3, 2));

    assert(Q(1, 2) + Q(1, 3) == Q(5, 6));
}
```

## Value passing

### Value semantics with explicit copying and move

For `MpZ`, copy construction is disabled by default to prevent inadvertent
copying. Instead use `f(move(z))` or `f(z.move)` (from `std.algorithm.mutation`)
to pass by move or `f(z.dup)` to pass by explicit copy (via `MpZ`'s member
function `.dup`).

### Value semantics with copy-on-write a la Phobos’ `std.bigint.BigInt`

Use `Z` (`CopyableMpZ`), for a drop-in-replacement for Phobos’
`std.bigint.BigInt`. For reference see
https://dlang.org/phobos/std_bigint.html#.BigInt.

## Mappings to GNU MP C library

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

## Compilation Performance

Compilation is fast; DMD compiles `gmp-d` in about 0.1 seconds on a modern
machine.

## Run-time Performance

Wrapper is as light-weight as possible; Some D-functions directly access the
internal C-datastructure exposed by the structs and macros in the C-headers of
GNU GMP.

## Limitations

Note that D's `__traits(isRef)` currently cannot be used to distinguish l-value
from r-value passing of `this`. This severly limits the possibilities of using
C++-style [expression
templates](https://en.wikipedia.org/wiki/Expression_templates) to realize lazy
evaluation in operator overloading. If this limitation were to be fixed
(probably via some introspection mechanism other than the trait `isRef` or
non-`struct`-member operator overloading), this library could implement lowering
of expressions such

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

There are more `mpz_t`- and `mpq_t`-functions that should be wrapped but these are good start.
