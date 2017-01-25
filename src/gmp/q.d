/// Multiple precision rational numbers (Q).
module gmp.q;

import gmp.traits;
import gmp.z;

/** Arbitrary (multi) precision rational number (Q).
    Wrapper for GNU MP (GMP)'s type `mpq_t` and functions `__gmpq_.*`.
 */
struct MpQ
{
    pure nothrow pragma(inline, true):

    /// Convert to `string` in base `base`.
    string toString(uint base = defaultBase,
                    bool upperCaseDigits = false) const @trusted
    {
        assert((base >= -2 && base <= -36) ||
               (base >= 2 && base <= 62));

        assert(false, "TODO");
    }

    // TODO toRCString wrapped in UniqueRange

    /// Returns: A unique hash of the `Mpq` value suitable for use in a hash table.
    size_t toHash() const
    {
        assert(false, "TODO");
    }

    @nogc:

    /** No default construction for now, because `mpq_init` initialize
        `__mpq_struct`-fields to non-zero values.

        TODO Allow default construction by delaying call to initialize().
    */
    @disable this();

    /// No copy construction.
    @disable this(this);

    /// Construct empty (undefined) from explicit `null`.
    this(typeof(null)) @safe
    {
        initialize();
    }

    /** Construct from floating-point `value`.
     */
    this(P)(P value) @trusted
        if (isFloating!P)
    {
        initialize();
        this = value;           // reuse opAssign
    }

    /** Construct from `pValue` / `qValue`.

        Note that `qValue` must be explicitly given, to prevent accidental
        storage of integers as rations with denominator being 1.
     */
    this(P, Q)(P pValue, Q qValue,
               bool canonicalizeFlag = false) @trusted
        if (isIntegral!P &&
            isIntegral!Q)
    {
        initialize();

        version(ccc) ++_ccc;

        static if (isSigned!Q)
        {
            assert(qValue >= 1, "Negative denominator");
        }

        // dln("qValue:", qValue);

        static      if (isUnsigned!P)
        {
            // dln("unsigned pValue:", pValue);
            __gmpq_set_ui(_ptr, pValue, qValue);
        }
        else                    // signed integral
        {
            // dln("signed pValue:", pValue);
            __gmpq_set_si(_ptr, pValue, qValue);
        }

        if (canonicalizeFlag) { canonicalize(); }
    }

    /** Construct from floating-point `value`.
     */
    ref MpQ opAssign(P)(P value) @trusted // TODO scope
        if (isFloating!P)
    {
        version(ccc) ++_ccc;
        __gmpq_set_d(_ptr, value);
        return this;
    }

    /** Assign from integer `value`. */
    ref MpQ opAssign(P)(P value) @trusted // TODO scope
        if (isIntegral!P)
    {
        version(ccc) ++_ccc;

        static      if (isUnsigned!P)
            __gmpq_set_ui(_ptr, value, 1);
        else                    // signed integral
            __gmpq_set_si(_ptr, value, 1);

        return this;
    }

    /** Canonicalize `this`. */
    void canonicalize() @trusted
    {
        __gmpq_canonicalize(_ptr); version(ccc) ++_ccc;
    }

    /// Destruct `this`.
    ~this() @trusted
    {
        assert(_ptr, "Pointer is null");
        __gmpq_clear(_ptr); version(ccc) ++_ccc;
    }

    /// Returns: numerator reference of `this`.
    @property ref inout(MpZ) numerator() @trusted inout return // TODO scope
    {
        return *(cast(inout(MpZ)*)_num_ptr);
    }

    /// Returns: denominator reference of `this`.
    @property ref inout(MpZ) denominator() @trusted inout return // TODO scope
    {
        return *(cast(inout(MpZ)*)_den_ptr);
    }

    /// Returns: the integer part of `this`, with any remainder truncated.
    @property MpZ integerPart() @safe
    {
        return numerator / denominator;
    }

    /// Returns: the fractional part of `this`.
    // TODO activate when sub(MpQ, MpZ) has been added
    // @property MpQ fractionPart()
    // {
    //     return this - integerPart;
    // }

    /// Cast to arithmetic type `T`.
    T opCast(T)() const @trusted // TODO scope
        if (isFloating!T)
    {
        return cast(T)__gmpq_get_d(_ptr);
    }

    /** Invert `this` in-place.
        Returns: `void` to make it obvious that `this` is mutated.
     */
    void invert() @trusted
    {
        import std.algorithm.mutation : swap;
        const bool negative = numerator < 0;
        if (negative)
        {
            numerator.absolute();         // fast inline
            swap(numerator, denominator); // fast inline
            numerator.negate();           // fast inline
        }
        else
        {
            swap(numerator, denominator); // fast inline
        }

    }

    /** Make `this` the absolute value of itself in-place.
        Returns: `void` to make it obvious that `this` is mutated.
     */
    void absolute() @trusted
    {
        numerator.absolute();
    }

private:

    /// Default conversion base.
    enum defaultBase = 10;

    /** Initialize internal struct. */
    private void initialize() @trusted // cannot be called `init` as that will override builtin type property
    {
        __gmpq_init(_ptr); version(ccc) ++_ccc;
    }

    /// Returns: pointer to internal rational C struct.
    inout(__mpq_struct)* _ptr() inout return @system // TODO scope
    {
        return &_q;
    }

    /// Returns: pointer to internal numerator C struct.
    inout(__mpz_struct)* _num_ptr() inout return @system // TODO scope
    {
        return cast(typeof(return))&_q._mp_num;
    }

    /// Returns: pointer to internal denominator C struct.
    inout(__mpz_struct)* _den_ptr() inout return @system // TODO scope
    {
        return cast(typeof(return))&_q._mp_den;
    }

    __mpq_struct _q;            // internal libgmp C struct

    // qualified C memory managment
    static @safe
    {
        pragma(mangle, "malloc") void* qualifiedMalloc(size_t size);
        pragma(mangle, "free") void qualifiedFree(void* ptr);
    }

    version(ccc)
    {

        /** Number of calls made to `__gmpq`--functions that construct or changes
            this value. Used to verify correct lowering and evaluation of template
            expressions.

            For instance the `x` in `x = y + z` should be assigned only once inside
            a call to `mpq_add`.
        */
        @property size_t mutatingCallCount() const @safe { return _ccc; }
        size_t _ccc;  // C mutation call count. number of calls to C GMP function calls that mutate this object
    }
}

pure nothrow:

/// construction and assignment
@safe @nogc unittest
{
    Q x = null;
    assert(x.numerator == 0);
    assert(x.denominator == 1);

    const Q y = Q(11, 13UL);
    assert(y.numerator == 11);
    assert(y.denominator == 13);

    immutable Q z = Q(7UL, 13UL);
    assert(z.numerator == 7);
    assert(z.denominator == 13);

    Q w = 0.25;                 // construct from floating point
    assert(w.numerator == 1);
    assert(w.denominator == 4);

    w = 0.125;                  // assign from floating point
    assert(w.numerator == 1);
    assert(w.denominator == 8);

    w = 2;                      // assign from integral
    assert(w.numerator == 2);
    assert(w.denominator == 1);

    w = 3;                      // assign from integral
    assert(w.numerator == 3);
    assert(w.denominator == 1);
}

/// canonicalization
@safe @nogc unittest
{
    Q x = Q(2, 4);
    assert(x.numerator == 2);
    assert(x.denominator == 4);
    x.canonicalize();
    assert(x.numerator == 1);
    assert(x.denominator == 2);
}

/// inversion
@safe unittest
{
    Q x = Q(1, 2);
    assert(x.numerator == 1);
    assert(x.denominator == 2);

    x.invert();
    assert(x.numerator == 2);
    assert(x.denominator == 1);

    Q y = Q(-1, 2);

    // TODO:
    // assert(x.numerator == -1);
    // assert(x.denominator == 2);

    // x.invert();
    // assert(x.numerator == -2);
    // assert(x.denominator == 1);
}

/// integer and fractional part
@safe unittest
{
    Q x = Q(5, 2);
    assert(x.integerPart == 2);

    x = Q(7, 2);
    assert(x.integerPart == 3);

    x = Q(10, 2);
    assert(x.integerPart == 5);
}

/// casting
@safe @nogc unittest
{
    assert(cast(double)Q(1, 2) == 0.5);
    assert(cast(double)Q(2, 4) == 0.5);
    assert(cast(double)Q(1, 8) == 1.0/8);
}

version(unittest)
{
    import dbgio : dln;
    alias Z = MpZ;
    alias Q = MpQ;
    debug import core.stdc.stdio : printf;
    version = ccc;              // do C mutation call count
    static assert(!isMpZExpr!int);
    import std.meta : AliasSeq;
}

// C API
extern(C)
{
    struct __mpq_struct
    {
        __mpz_struct _mp_num;
        __mpz_struct _mp_den;
    }
    static assert(__mpq_struct.sizeof == 32); // fits in four 64-bit words

    alias mpq_srcptr = const(__mpq_struct)*;
    alias mpq_ptr = __mpq_struct*;
    alias mp_bitcnt_t = ulong;

    pure nothrow @nogc:

    void __gmpq_init (mpq_ptr);
    void __gmpq_clear (mpq_ptr);

    void __gmpq_set (mpq_ptr, mpq_srcptr);
    void __gmpq_set_z (mpq_ptr, mpz_srcptr);

    void __gmpq_set_ui (mpq_ptr, ulong, ulong);
    void __gmpq_set_si (mpq_ptr, long, ulong);
    void __gmpq_set_d (mpq_ptr, double);

    double __gmpq_get_d (mpq_srcptr);

    void __gmpq_canonicalize (mpq_ptr);
}

pragma(lib, "gmp");
