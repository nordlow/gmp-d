/** High-level wrapper for GNU Multiple Precision (MP) library.
    See also: http://www.dsource.org/projects/bindings/browser/trunk/gmp
 */
module gmp;

import std.stdio : writeln;
debug import core.stdc.stdio : printf;

enum unittestLong = false;
version = unittestPhobos;

version(unittest)
{
    import dbgio;
}

// import deimos.gmp.gmp;
// import deimos.gmp.integer;

/** Arbitrary precision signed integer (Z).
 */
struct Integer
{
    import std.typecons : Unqual;
    import std.traits : isSigned, isUnsigned, isIntegral;

    /// Default conversion base.
    private enum defaultBase = 10;

    pragma(inline)
    @trusted pure nothrow:

    /// Convert to string in base `base`.
    string toString(int base = defaultBase) const
    {
        assert(base >= -2 && base <= -36 ||
               base >= 2 && base <= 62);
        immutable size = sizeInBase(base);
        string str = new string(size + 1); // one extra for minus sign
        __gmpz_get_str(cast(char*)str.ptr, base, _ptr);
        return str[0] == '-' ? str : str.ptr[0 .. size];
    }

    // TODO toRCString wrapped in UniqueRange

    @nogc:

    /// No default construction.
    @disable this();

    /// Construct empty (undefined) from explicit `null`.
    this(typeof(null))
    {
        initialize();           // TODO remove if this is same as zero bitblit
        assert(this == Integer.init); // if this is same as default
    }

    /// Construct from `value`.
    this(long value) { __gmpz_init_set_si(_ptr, value); }
    /// ditto
    this(ulong value) { __gmpz_init_set_ui(_ptr, value); }
    /// ditto
    this(double value) { __gmpz_init_set_d(_ptr, value); } // TODO Use Optional/Nullable when value is nan, or inf
    /// ditto
    this(int value) { this(cast(long)value); }
    /// ditto
    this(uint value) { this(cast(ulong)value); }

    /// Construct from `value` in base `base`.
    this(const string value, int base = 0) // TODO Use Optional/Nullable when value is nan, or inf
    {
        assert(base == 0 || base >= 2 && base <= 62);

        // @nogc variant of `toStringz` with scoped heap allocation of null-terminated C-string `stringz`
        char* stringz = cast(char*)qualifiedMalloc(value.length + 1); // maximum this many characters
        size_t i = 0;
        foreach (immutable char ch; value[])
        {
            if (ch != '_')
            {
                stringz[i] = ch;
                i += 1;
            }
        }
        stringz[i] = '\0'; // set C null terminator

        const int status = __gmpz_init_set_str(_ptr, stringz, base);

        qualifiedFree(stringz);
        assert(status == 0, "Parameter `value` does not contain an integer");
    }

    enum useCopy = false;       // disable copy construction for now
    static if (useCopy)
    {
        this()(auto ref const Integer value)
        {
            mpz_init_set(_ptr, value._pt);
        }
    }
    else
    {
        /// Disable copy construction.
        @disable this(this);
    }

    /** Initialize internal struct.
        Cannot be called `init` as that will override builtin type property.
     */
    private void initialize()
    {
        __gmpz_init(_ptr);
    }

    /// Swap content of `this` with `rhs`.
    void swap(ref Integer rhs)
    {
        __gmpz_swap(_ptr, rhs._ptr);
    }

    /// Returns: (duplicate) copy of `this`.
    Integer dup() const
    {
        typeof(return) y = void;
        __gmpz_init_set(y._ptr, _ptr);
        return y;
    }

    /// Destruct `this`.
    ~this() { if (_ptr) { __gmpz_clear(_ptr); } }

    /// Returns: `true` iff `this` equals `rhs`.
    bool opEquals()(auto ref const Integer rhs) const
    {
        return (_ptr == rhs._ptr || // fast compare
                __gmpz_cmp(_ptr, rhs._ptr) == 0);
    }
    /// ditto
    bool opEquals(double rhs) const { return __gmpz_cmp_d(_ptr, rhs) == 0; }
    /// ditto
    bool opEquals(long rhs) const { return __gmpz_cmp_si(_ptr, rhs) == 0; }
    /// ditto
    bool opEquals(ulong rhs) const { return __gmpz_cmp_ui(_ptr, rhs) == 0; }
    /// ditto
    bool opEquals(int rhs) const { return opEquals(cast(long)rhs); }
    /// ditto
    bool opEquals(uint rhs) const { return opEquals(cast(ulong)rhs); }

    // comparison
    int opCmp()(auto ref const Integer rhs) const { return __gmpz_cmp(_ptr, rhs._ptr); }
    /// ditto
    int opCmp(double rhs) const { return __gmpz_cmp_d(_ptr, rhs); }
    /// ditto
    int opCmp(long rhs) const { return __gmpz_cmp_si(_ptr, rhs); }
    /// ditto
    int opCmp(ulong rhs) const { return __gmpz_cmp_ui(_ptr, rhs); }
    /// ditto
    int opCmp(int rhs) const { return opCmp(cast(long)rhs); }
    /// ditto
    int opCmp(uint rhs) const { return opCmp(cast(ulong)rhs); }

    /// Cast to `ulong`.
    ulong opCast(T : ulong)() const { return __gmpz_get_ui(_ptr); }

    Integer opBinary(string s)(auto ref const Integer rhs) const
        if (s == "+" || s == "-" || s == "*" || s == "/" || s == "%")
    {
        typeof(return) y = null;
        static      if (s == "+")
        {
            __gmpz_add(y._ptr, _ptr, rhs._ptr);
        }
        else static if (s == "-")
        {
            __gmpz_sub(y._ptr, _ptr, rhs._ptr);
        }
        else static if (s == "*")
        {
            __gmpz_mul(y._ptr, _ptr, rhs._ptr);
        }
        else static if (s == "/")
        {
            __gmpz_tdiv_q(y._ptr, _ptr, rhs._ptr);
        }
        else static if (s == "%")
        {
            __gmpz_mod(y._ptr, _ptr, rhs._ptr);
        }
        else
        {
            static assert(false);
        }
        return y;
    }

    Integer opBinary(string s, Unsigned)(Unsigned rhs) const
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "^^") &&
            isUnsigned!Unsigned)
    {
        typeof(return) y = null;
        static      if (s == "+")
        {
            __gmpz_add_ui(y._ptr, _ptr, rhs);
        }
        else static if (s == "-")
        {
            __gmpz_sub_ui(y._ptr, _ptr, rhs);
        }
        else static if (s == "*")
        {
            __gmpz_mul_ui(y._ptr, _ptr, rhs);
        }
        else static if (s == "/")
        {
            __gmpz_tdiv_q_ui(y._ptr, _ptr, rhs);
        }
        else static if (s == "^^")
        {
            __gmpz_pow_ui(y._ptr, _ptr, rhs);
        }
        else
        {
            static assert(false);
        }
        return y;
    }

    Integer opBinary(string s, Signed)(Signed rhs) const
        if ((s == "+" || s == "-" || s == "*" || s == "^^") &&
            isSigned!Signed)
    {
        typeof(return) y = null;
        static      if (s == "+")
        {
            __gmpz_add_si(y._ptr, _ptr, rhs);
        }
        else static if (s == "-")
        {
            // TODO handle case `rhs.min`, as `-rhs.min` cannot be represented as a `Signed`
            if (rhs < 0)
            {
                __gmpz_add_ui(y._ptr, _ptr, -rhs); // x - (-y) == x + y
            }
            else
            {
                __gmpz_sub_ui(y._ptr, _ptr, rhs); // rhs is positive
            }
        }
        else static if (s == "*")
        {
            __gmpz_mul_si(y._ptr, _ptr, rhs);
        }
        else static if (s == "^^")
        {
            if (rhs < 0)
            {
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_pow_ui(y._ptr, _ptr, pos_rhs); // use positive power
                if (pos_rhs & 1)                      // if odd power
                {
                    __gmpz_neg(y._ptr, y._ptr); // negate result
                }
            }
            else
            {
                __gmpz_pow_ui(y._ptr, _ptr, rhs);
            }
        }
        else
        {
            static assert(false);
        }
        return y;
    }

    /// Remainer propagates modulus type.
    Unqual!Integral opBinary(string s, Integral)(Integral rhs) const
        if ((s == "%") &&
            isIntegral!Integral)
    {
        Integer y = null;
        static if (isSigned!Integral)
        {
            if (rhs < 0)
            {
                return cast(typeof(return))__gmpz_tdiv_r_ui(y._ptr, _ptr, rhs);
            }
            else
            {
                return cast(typeof(return))__gmpz_tdiv_r_ui(y._ptr, _ptr, rhs);
            }
        }
        else static if (isUnsigned!Integral)
        {
            return cast(typeof(return))__gmpz_tdiv_r_ui(y._ptr, _ptr, rhs);
        }
    }

    ref Integer opOpAssign(string s)(auto ref const Integer rhs)
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%"))
    {
        static      if (s == "+")
        {
            __gmpz_add(_ptr, _ptr, rhs._ptr);
        }
        else static if (s == "-")
        {
            __gmpz_sub(_ptr, _ptr, rhs._ptr);
        }
        else static if (s == "*")
        {
            __gmpz_mul(_ptr, _ptr, rhs._ptr);
        }
        else static if (s == "/")
        {
            __gmpz_tdiv_q(_ptr, _ptr, rhs._ptr);
        }
        else static if (s == "%")
        {
            __gmpz_tdiv_r(_ptr, _ptr, rhs._ptr);
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    ref Integer opOpAssign(string s, Unsigned)(Unsigned rhs)
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%" || s == "^^") &&
            isUnsigned!Unsigned)
    {
        static      if (s == "+")
        {
            __gmpz_add_ui(_ptr, _ptr, rhs);
        }
        else static if (s == "-")
        {
            __gmpz_sub_ui(_ptr, _ptr, rhs);
        }
        else static if (s == "*")
        {
            __gmpz_mul_ui(_ptr, _ptr, rhs);
        }
        else static if (s == "/")
        {
            __gmpz_tdiv_q_ui(_ptr, _ptr, rhs);
        }
        else static if (s == "%")
        {
            __gmpz_tdiv_r_ui(_ptr, _ptr, rhs);
        }
        else static if (s == "^^")
        {
            __gmpz_pow_ui(_ptr, _ptr, rhs);
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    ref Integer opOpAssign(string s, Signed)(Signed rhs)
        if ((s == "+" || s == "-" || s == "*") &&
            isSigned!Signed)
    {
        static      if (s == "+")
        {
            __gmpz_add_si(_ptr, _ptr, rhs);
        }
        else static if (s == "-")
        {
            __gmpz_sub_si(_ptr, _ptr, rhs);
        }
        else static if (s == "*")
        {
            __gmpz_mul_si(_ptr, _ptr, rhs);
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    /// Returns: negation of `this`.
    Integer opUnary(string s)() const
        if (s == "-")
    {
        typeof(return) y = null;
        __gmpz_neg(y._ptr, _ptr);
        return y;
    }

    /// Returns: `base` raised to the power of `exp`.
    static Integer pow(ulong base, ulong exp)
    {
        typeof(return) y = null;
        __gmpz_ui_pow_ui(y._ptr, base, exp);
        return y;
    }

    /** Returns: `this` ^^ `power` (mod `modulo`).
        TODO can we somehow capture lazy evaluation?
     */
    Integer powm()(auto ref const Integer power,
                   auto ref const Integer modulo) const
    {
        typeof(return) rop = 0; // result
        __gmpz_powm(rop._ptr,
                    _ptr,
                    power._ptr,
                    modulo._ptr);
        return rop;
    }
    /// ditto
    Integer powm()(ulong power,
                   auto ref const Integer modulo) const
    {
        typeof(return) rop = 0;       // result
        __gmpz_powm_ui(rop._ptr,
                       _ptr,
                       power,
                       modulo._ptr);
        return rop;
    }

    /// Returns: number of digits in base `base`.
    size_t sizeInBase(int base) const
    {
        return __gmpz_sizeinbase(_ptr, base);
    }

private:

    /// Returns: pointer to internal GMP C struct.
    inout(__mpz_struct)* _ptr() inout { return &_z; }

    __mpz_struct _z;            // internal libgmp C struct

    // qualified C memory managment
    static @safe pure nothrow @nogc
    {
        pragma(mangle, "malloc") void* qualifiedMalloc(size_t size);
        pragma(mangle, "free") void qualifiedFree(void* ptr);
    }

}

pure nothrow:

/// convert to string
@safe unittest
{
    alias Z = Integer;
    assert(Z(42).toString == `42`);
    assert(Z(-42).toString == `-42`);
    assert(Z(`-101`).toString == `-101`);
}

/// Returns: absolute value of `x`.
pragma(inline)
Integer abs(const ref Integer x) @trusted @nogc
{
    typeof(return) y = null;
    __gmpz_abs(y._ptr, x._ptr);
    return y;
}

/// Swap contents of `x` with contents of `y`.
pragma(inline)
void swap(ref Integer x, ref Integer y) @trusted @nogc
{
    x.swap(y);
}

/// Returns: subtraction `x` - `y`.
/// TODO use http://dlang.org/phobos/std_bigint.html#.BigInt.opBinaryRight instead
import std.traits : isUnsigned;
pragma(inline)
Integer opBinary(string s, Unsigned)(Unsigned x, auto ref const Integer y) @trusted @nogc
    if (s == "-" &&
        isUnsigned!Unsigned)
{
    typeof(return) rop = null;
    __gmpz_ui_sub(rop._ptr, x, y._ptr);
    return rop;
}

///
@safe @nogc unittest
{
    alias Z = Integer;

    const Z _ = cast(uint)42;
    const Z a = 42;
    const Z b = 43UL;
    const Z c = 43.0;

    // binary
    assert(Z(`0b11`) == 3);
    assert(Z(`0B11`) == 3);

    // octal
    assert(Z(`07`) == 7);
    assert(Z(`010`) == 8);

    // hexadecimal
    assert(Z(`0x10`) == 16);
    assert(Z(`0X10`) == 16);

    // decimal
    assert(Z(`101`) == 101);
    assert(Z(`101`, 10) == 101);

    immutable Z ic = 101UL;

    assert(a == a.dup);
    assert(ic == ic.dup);

    // equality
    assert(a == a);
    assert(a == Z(42UL));
    assert(a == 42.0);
    assert(a == 42);
    assert(a == cast(uint)42);
    assert(a == 42UL);
    assert(_ == 42);
    assert(c == 43.0);

    // non-equality
    assert(a != b);

    // less than
    assert(a < b);
    assert(a < Z(43UL));
    assert(a < 43);
    assert(a < cast(uint)43);
    assert(a < 43UL);
    assert(a < 43.0);

    // greater than
    assert(b > a);
    assert(b > Z(42UL));
    assert(b > 42);
    assert(b > cast(uint)42);
    assert(b > 42UL);
    assert(b > 42.0);

    // absolute value
    assert(abs(a) == a);
    assert(a.abs == a);         // UFCS

    // negated value
    assert(-a == -42);
    assert(-(-a) == a);

    // addition
    assert(a + b == b + a);
    assert(a + Z(43UL) == b + a);
    assert(a + 0UL == a);
    assert(a + 1UL != a);
    assert(a + b == 42UL + 43UL);

    // subtraction
    assert(a - 2 == 40);
    assert(a - (-2) == 44);
    assert(opBinary!"-"(44UL, Z(42)) == 2);
    // assert(44UL - Z(42) == 2); // TODO why does this fail when this doesn't: opBinary!"-"(44UL, Z(42))

    // multiplication
    assert(a * 1UL == a);
    assert(a * 1 == a);
    assert(a * 2 != a);
    assert(a * b == b * a);
    assert(a * b == 42UL * 43UL);

    // modulo/remainder
    immutable Z one = 1UL;
    const Z two = 2UL;
    immutable Z three = 3UL;
    const Z four = 4UL;
    immutable Z five = 5UL;
    const Z six = 6UL;
    assert(six % one == 0);
    assert(six % two == 0);
    assert(six % three == 0);
    assert(six % four == 2);
    assert(six % five == 1);
    assert(six % six == 0);

    // subtraction
    assert(six - one == 5);
    assert(six - 1UL == 5);
    assert(six - 1 == 5);
    // TODO assert(1UL - six == -5);

    // exponentiation
    assert(Z(0)^^0 == 1);
    assert(Z(3)^^3 == 27);
    assert(Z(3)^^(-3) == -27);
    assert(Z(2)^^8 == 256);
    assert(Z(2)^^(-9) == -512);

    assert(Z.pow(2, 8) == 256);

    // exponentiation plus modulus
    assert(Z(2).powm(Z(8), Z(8)) == Z(0));
    assert(Z(2).powm(Z(3), Z(16)) == Z(8));
    assert(Z(3).powm(Z(3), Z(16)) == Z(11));

    assert(Z(2).powm(8, Z(8)) == Z(0));
    assert(Z(2).powm(3, Z(16)) == Z(8));
    assert(Z(3).powm(3, Z(16)) == Z(11));

    // swap
    Z x = 42;
    Z y = 43;
    assert(x == 42);
    assert(y == 43);
    x.swap(y);
    assert(y == 42);
    assert(x == 43);
    swap(x, y);
    assert(x == 42);
    assert(y == 43);
}

/// Phobos unittests
version(unittestPhobos) @safe @nogc unittest
{
    alias BigInt = Integer;     // Phobos naming convention
    {
        auto b = BigInt("1_000_000_000");

        b += 12345UL;
        assert(b == BigInt("1_000_012_345"));

        b /= 5UL;
        assert(b == BigInt("200_002_469"));
    }

    {
        auto x = BigInt("123");
        auto y = BigInt("321");
        x += y;
        assert(x == BigInt("444"));
    }

    {
        auto x = BigInt("123");
        auto y = BigInt("456");
        BigInt z = x * y;
        assert(z == BigInt("56088"));
    }

    {
        auto x = BigInt("123");
        x *= 300UL;
        assert(x == BigInt("36900"));
    }

    {
        auto  x  = BigInt("1_000_000_500");

        ulong ul  = 2_000_000UL;
        uint ui   = 500_000;
        ushort us = 30_000;
        ubyte ub  = 50;

        long  l = 1_000_000L;
        int   i = 500_000;
        short s = 30_000;
        byte b  = 50;

        static assert(is(typeof(x % ul)  == ulong));
        static assert(is(typeof(x % ui)  == uint));
        static assert(is(typeof(x % us)  == ushort));
        static assert(is(typeof(x % ub)  == ubyte));

        static assert(is(typeof(x % l)  == long));
        static assert(is(typeof(x % i)  == int));
        static assert(is(typeof(x % s)  == short));
        static assert(is(typeof(x % b)  == byte));

        assert(x % ul == BigInt(500));
        assert(x % ui == BigInt(500));
        assert(x % us  == 10500);
        assert(x % ub == 0);

        assert(x % l  == 500L);
        assert(x % i  == 500);
        assert(x % s  == 10500);
        assert(x % b == 0);
    }
}

// Fermats Little Theorem
pure unittest
{
    // calculate a mersenne prime, M(p) = 2 ^ p - 1
    Integer M(ulong p)
    {
        typeof(return) x = 2UL;
        x ^^= p;
        return x - 1;
    }

    if (unittestLong) // compile but not run unless flagged for because running is slow
    {
/*
  Fermats little theorem: a ^ p ≡ a (mod p) ∀ prime p
  check Fermats little theorem for a ≤ 100000 and all mersene primes M(p) : p ≤ 127
*/
        foreach (immutable ulong i; [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127])
        {
            foreach (immutable ulong j; 2 .. 100000)
            {
                const p = M(i);       // power
                const a = Integer(j); // base
                const amp = a % p;
                const b = a.powm(p, p); // result
                assert(b == amp);
            }
        }
    }
}

// Euler's Sum of Powers Conjecture counter example
pure unittest
{
    /*
      a^5 + b^5 + c^5 + d^5 = e^5
      Lander & Parkin, 1966 found the first counter example:
      27^5 + 84^5 + 110^5 + 133^5 = 144^5
      this test is going to search for this counter example by
      brute force for all positive a, b, c, d ≤ 200
    */

    enum LIMIT = 200;
    enum POWER = 5;

    if (unittestLong) // compile but not run unless flagged for because running is slow
    {
        bool found = false;
        Integer r1 = 0;
    outermost:
        foreach (immutable ulong a; 1 .. LIMIT)
        {
            foreach (immutable ulong b; a .. LIMIT)
            {
                foreach (immutable ulong c; b .. LIMIT)
                {
                    foreach (immutable ulong d; c .. LIMIT)
                    {
                        r1 = ((Integer(a) ^^ POWER) +
                              (Integer(b) ^^ POWER) +
                              (Integer(c) ^^ POWER) +
                              (Integer(d) ^^ POWER));
                        Integer rem = 0;
                        __gmpz_rootrem(r1._ptr,
                                       rem._ptr,
                                       r1._ptr,
                                       POWER);
                        if (rem == 0UL)
                        {
                            debug printf("Counter Example Found: %lu^5 + %lu^5 + %lu^5 + %lu^5 = %lu^5\n",
                                         a, b, c, d, cast(ulong)r1);
                            found = true;
                            break outermost;
                        }
                    }
                }
            }
        }
        assert(found);
    }
}

// C API
extern(C)
{
    struct __mpz_struct
    {
        int _mp_alloc;		/* Number of *limbs* allocated and pointed to by
                                   the _mp_d field.  */
        int _mp_size;           /* abs(_mp_size) is the number of limbs the last
                                   field points to.  If _mp_size is negative
                                   this is a negative number.  */
        void* _mp_d;            /* Pointer to the limbs.  */
    }

    alias mpz_srcptr = const(__mpz_struct)*;
    alias mpz_ptr = __mpz_struct*;
    alias mp_bitcnt_t = ulong;

    pure nothrow @nogc:

    void __gmpz_init (mpz_ptr);
    void __gmpz_init_set (mpz_ptr, mpz_srcptr);

    void __gmpz_init_set_d (mpz_ptr, double);
    void __gmpz_init_set_si (mpz_ptr, long);
    void __gmpz_init_set_ui (mpz_ptr, ulong);
    int __gmpz_init_set_str (mpz_ptr, const char*, int);

    void __gmpz_clear (mpz_ptr);

    void __gmpz_abs (mpz_ptr, mpz_srcptr);
    void __gmpz_neg (mpz_ptr, mpz_srcptr);

    void __gmpz_add (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_add_ui (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_addmul (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_addmul_ui (mpz_ptr, mpz_srcptr, ulong);

    void __gmpz_sub (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_sub_ui (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_ui_sub (mpz_ptr, ulong, mpz_srcptr);

    void __gmpz_mul (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_mul_2exp (mpz_ptr, mpz_srcptr, mp_bitcnt_t);
    void __gmpz_mul_si (mpz_ptr, mpz_srcptr, long);
    void __gmpz_mul_ui (mpz_ptr, mpz_srcptr, ulong);

    void __gmpz_cdiv_q_ui (mpz_ptr, mpz_srcptr, ulong);
    ulong __gmpz_cdiv_r_ui (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_cdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_cdiv_ui (mpz_srcptr, ulong);

    void __gmpz_fdiv_q_ui (mpz_ptr, mpz_srcptr, ulong);
    ulong __gmpz_fdiv_r_ui (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_fdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_fdiv_ui (mpz_srcptr, ulong);

    void __gmpz_tdiv_q_ui (mpz_ptr, mpz_srcptr, ulong);
    ulong __gmpz_tdiv_r_ui (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_tdiv_qr_ui (mpz_ptr, mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_tdiv_ui (mpz_srcptr, ulong);

    void __gmpz_mod (mpz_ptr, mpz_srcptr, mpz_srcptr);

    void __gmpz_pow_ui (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_ui_pow_ui (mpz_ptr, ulong, ulong);

    void __gmpz_swap (mpz_ptr, mpz_ptr); // TODO: __GMP_NOTHROW;

    int __gmpz_cmp (mpz_srcptr, mpz_srcptr); // TODO: __GMP_NOTHROW __GMP_ATTRIBUTE_PURE;
    int __gmpz_cmp_d (mpz_srcptr, double); // TODO: __GMP_ATTRIBUTE_PURE
    int __gmpz_cmp_si (mpz_srcptr, long); // TODO: __GMP_NOTHROW __GMP_ATTRIBUTE_PURE
    int __gmpz_cmp_ui (mpz_srcptr, ulong); // TODO: __GMP_NOTHROW __GMP_ATTRIBUTE_PURE

    char *__gmpz_get_str (char*, int, mpz_srcptr);
    size_t __gmpz_sizeinbase (mpz_srcptr, int); // TODO __GMP_NOTHROW __GMP_ATTRIBUTE_PURE;

    void __gmpz_powm (mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_powm_ui (mpz_ptr, mpz_srcptr, ulong, mpz_srcptr);

    ulong __gmpz_get_ui (mpz_srcptr);

    // TODO wrap:
    void __gmpz_root (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_rootrem (mpz_ptr, mpz_ptr, mpz_srcptr, ulong);

    void __gmpz_sqrt (mpz_ptr, mpz_srcptr);
    void __gmpz_sqrtrem (mpz_ptr, mpz_ptr, mpz_srcptr);

    void __gmpz_perfect_power_p (mpz_ptr, mpz_srcptr);
}

// link with C library GNU MP
pragma(lib, "gmp");
