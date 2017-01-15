/** High-level wrapper for GNU Multiple Precision (MP) library.
    See also: http://www.dsource.org/projects/bindings/browser/trunk/gmp
 */
module gmp;

debug import core.stdc.stdio : printf;
import std.typecons : Unsigned;

enum unittestLong = false;
version = unittestPhobos;

/** Evaluation Policy. */
enum Eval
{
    direct,                     // direct eval
    delayed,                    // lazy eval via expression templates
}

/** Is `true` iff `T` is a GNU MP arithmetic type (`long`, `ulong` or `double`). */
enum isGMPArithmetic(T) = is(T == long) && is(T == ulong) && is(T == double);

// TODO use these imports instead of the ones below
// import deimos.gmp.gmp;
// import deimos.gmp.integer;

/** Arbitrary (multi) precision signed integer (Z).
    Wrapper for GNU MP (GMP)'s type `mpz_t` and functions `mpz_.*`.
 */
struct MpZ(Eval eval = Eval.direct)
{
    import std.typecons : Unqual;

    /// Default conversion base.
    private enum defaultBase = 10;

    @trusted pure nothrow pragma(inline, true):

    /// Convert to `string` in base `base`.
    string toString(int base = defaultBase, bool upperCaseDigits = false) const
    {
        assert((base >= -2 && base <= -36) ||
               (base >= 2 && base <= 62));

        immutable size = sizeInBase(base);

        string str = new string(size + 1); // one extra for minus sign
        __gmpz_get_str(cast(char*)str.ptr, base, _ptr); // fill it

        if (upperCaseDigits)
        {
            foreach (ref e; cast(char[])str)
            {
                import std.ascii : isLower, toUpper;
                if (e.isLower)
                {
                    e = e.toUpper;
                }
            }
        }

        return str[0] == '-' ? str : str.ptr[0 .. size];
    }

    // TODO toRCString wrapped in UniqueRange

    // Returns: A unique hash of the `MpZ` value suitable for use in a hash table.
    size_t toHash() const
    {
        import core.internal.hash : hashOf;
        typeof(return) hash = _limbCount;
        foreach (immutable i; 0 .. _limbCount)
        {
            hash ^= _limbs[i].hashOf;
        }
        return hash;
    }

    @nogc:

    /// No default construction.
    @disable this();

    /// Construct empty (undefined) from explicit `null`.
    this(typeof(null))
    {
        initialize();             // TODO is there a faster way?
        assert(this == MpZ.init); // if this is same as default
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

    /** Construct from `value` in base `base`.
        If `base` is 0 it's guessed from contents of `value`.
        */
    pragma(inline, false)
    this(in string value, int base = 0) // TODO Use Optional/Nullable when value is nan, or inf
    {
        assert(base == 0 || (base >= 2 && base <= 62));
        char* stringz = _allocStringzCopyOf(value);
        immutable int status = __gmpz_init_set_str(_ptr, stringz, base);
        qualifiedFree(stringz);
        assert(status == 0, "Parameter `value` does not contain an integer");
    }

    /// Returns: the Mersenne prime, M(p) = 2 ^^ p - 1
    static MpZ mersennePrime(Integral)(Integral p)
        if (isIntegral!Integral)
    {
        return typeof(this).pow(2UL, p) - 1;
    }

    /// Use copy construction.
    enum useCopy = false;       // disable copy construction for now

    static if (useCopy)
    {
        /// Construct copy of `value`.
        this()(auto ref const MpZ value)
        {
            mpz_init_set(_ptr, value._pt);
        }
    }
    else
    {
        /// Disable copy construction.
        @disable this(this);
    }

    /** Initialize internal struct. */
    private void initialize() // cannot be called `init` as that will override builtin type property
    {
        __gmpz_init(_ptr);
    }

    /// Swap content of `this` with `rhs`.
    void swap(ref MpZ rhs)
    {
        import std.algorithm.mutation : swap;
        swap(this, rhs); // faster than __gmpz_swap(_ptr, rhs._ptr);
    }

    /// Returns: (duplicate) copy of `this`.
    MpZ dup() const
    {
        typeof(return) y = void;
        __gmpz_init_set(y._ptr, _ptr);
        return y;
    }

    /// Assign from `rhs`.
    ref MpZ opAssign()(auto ref const MpZ rhs)
    {
        __gmpz_set(_ptr, rhs._ptr);
        return this;
    }
    /// ditto
    ref MpZ opAssign(Unsigned)(Unsigned rhs)
        if (isUnsigned!Unsigned)
    {
        __gmpz_set_ui(_ptr, rhs);
        return this;
    }
    /// ditto
    ref MpZ opAssign(Signed)(Signed rhs)
        if (isSigned!Signed)
    {
        __gmpz_set_si(_ptr, rhs);
        return this;
    }
    /// ditto
    ref MpZ opAssign(double rhs)
    {
        __gmpz_set_d(_ptr, rhs);
        return this;
    }

    /** Assign `this` from `string` `rhs` interpreted in base `base`.
        If `base` is 0 it's guessed from contents of `value`.
    */
    ref MpZ fromString(in string rhs, int base = 0) return // TODO DIP-1000 scope
    {
        assert(base == 0 || (base >= 2 && base <= 62));
        char* stringz = _allocStringzCopyOf(rhs);
        immutable int status = __gmpz_set_str(_ptr, stringz, base);
        qualifiedFree(stringz);
        assert(status == 0, "Parameter `rhs` does not contain an integer");
        return this;
    }

    /// Destruct `this`.
    ~this() { if (_ptr) { __gmpz_clear(_ptr); } }

    /// Returns: `true` iff `this` equals `rhs`.
    bool opEquals()(auto ref const MpZ rhs) const
    {
        return (_ptr == rhs._ptr || // fast equality
                __gmpz_cmp(_ptr, rhs._ptr) == 0);
    }

    // TODO use one common definition for all `Integral`
    /// ditto
    bool opEquals(long rhs) const
    {
        if (rhs == 0)
            return isZero;
        else
            return __gmpz_cmp_si(_ptr, rhs) == 0;
    }
    /// ditto
    bool opEquals(ulong rhs) const
    {
        if (rhs == 0)
            return isZero;
        else
            return __gmpz_cmp_ui(_ptr, rhs) == 0;
    }
    /// ditto
    bool opEquals(int rhs) const
    {
        if (rhs == 0)
            return isZero;
        else
            return opEquals(cast(long)rhs);
    }
    /// ditto
    bool opEquals(uint rhs) const
    {
        if (rhs == 0)
            return isZero;
        else
            return opEquals(cast(ulong)rhs);
    }

    /// ditto
    bool opEquals(double rhs) const
    {
        if (rhs == 0)
            return isZero;
        else
            return __gmpz_cmp_d(_ptr, rhs) == 0;
    }

    /// Compare `this` to `rhs`.
    int opCmp()(auto ref const MpZ rhs) const { return __gmpz_cmp(_ptr, rhs._ptr); }

    // TODO use one common definition for all `Integral`
    /// ditto
    int opCmp(long rhs) const
    {
        return __gmpz_cmp_si(_ptr, rhs);
    }
    /// ditto
    int opCmp(ulong rhs) const { return __gmpz_cmp_ui(_ptr, rhs); }
    /// ditto
    int opCmp(int rhs) const { return opCmp(cast(long)rhs); }
    /// ditto
    int opCmp(uint rhs) const { return opCmp(cast(ulong)rhs); }

    /// ditto
    int opCmp(double rhs) const { return __gmpz_cmp_d(_ptr, rhs); }

    /// Cast to `bool`.
    bool opCast(T : bool)() const { return !isZero; }

    /// Cast to unsigned type `T`.
    T opCast(T)() const if (isUnsigned!T) { return cast(T)__gmpz_get_ui(_ptr); }

    /// Cast to signed type `T`.
    T opCast(T)() const if (isSigned!T) { return cast(T)__gmpz_get_si(_ptr); }

    /** Returns: The value of this as a `long`, or +/- `long.max` if outside
        the representable range.
    */
    long toLong() const
    {
        // TODO can probably be optimized
        if (this <= long.min)
        {
            return long.min;
        }
        else if (this >= long.max)
        {
            return long.max;
        }
        else
        {
            return cast(long)__gmpz_get_si(_ptr);
        }
    }

    /** Returns: The value of this as a `int`, or +/- `int.max` if outside
        the representable range.
    */
    int toInt() const
    {
        // TODO can probably be optimized
        if (this <= int.min)
        {
            return int.min;
        }
        else if (this >= int.max)
        {
            return int.max;
        }
        else
        {
            return cast(int)__gmpz_get_si(_ptr);
        }
    }

    /// Cast to `double`.
    // TODO double opCast(T : double)() const { return __gmpz_get_d(_ptr); }

    MpZ opBinary(string s)(auto ref const MpZ rhs) const
        if (s == "+" || s == "-" || s == "*" || s == "/" || s == "%")
    {
        typeof(return) y = null;
        static      if (s == "+") { __gmpz_add(y._ptr, _ptr, rhs._ptr); }
        else static if (s == "-") { __gmpz_sub(y._ptr, _ptr, rhs._ptr); }
        else static if (s == "*") { __gmpz_mul(y._ptr, _ptr, rhs._ptr); }
        else static if (s == "/") { assert(rhs != 0, "Divison by zero");
                                    __gmpz_tdiv_q(y._ptr, _ptr, rhs._ptr); }
        else static if (s == "%")
        {
            // TODO use tdiv_r or mod?
            __gmpz_tdiv_r(y._ptr, _ptr, rhs._ptr);
            // __gmpz_mod(y._ptr, _ptr, rhs._ptr); // sign of divisor is ignored
        }
        else
        {
            static assert(false);
        }
        return y;
    }

    MpZ opBinary(string s, Unsigned)(Unsigned rhs) const
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "^^") &&
            isUnsigned!Unsigned)
    {
        typeof(return) y = null;
        static      if (s == "+") { __gmpz_add_ui(y._ptr, _ptr, rhs); }
        else static if (s == "-") { __gmpz_sub_ui(y._ptr, _ptr, rhs); }
        else static if (s == "*") { __gmpz_mul_ui(y._ptr, _ptr, rhs); }
        else static if (s == "/") { assert(rhs != 0, "Divison by zero");
                                    __gmpz_tdiv_q_ui(y._ptr, _ptr, rhs); }
        else static if (s == "^^") { __gmpz_pow_ui(y._ptr, _ptr, rhs); }
        else { static assert(false); }
        return y;
    }

    MpZ opBinary(string s, Signed)(Signed rhs) const
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "^^") &&
            isSigned!Signed)
    {
        typeof(return) y = null;
        static      if (s == "+")
        {
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_sub_ui(y._ptr, _ptr, pos_rhs);
            }
            else
            {
                __gmpz_add_ui(y._ptr, _ptr, rhs);
            }
        }
        else static if (s == "-")
        {
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
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
        else static if (s == "/")
        {
            assert(rhs != 0, "Divison by zero");
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_tdiv_q_ui(y._ptr, _ptr, pos_rhs);
                y.negate();     // negate result
            }
            else
            {
                __gmpz_tdiv_q_ui(y._ptr, _ptr, rhs);
            }
        }
        else static if (s == "^^")
        {
            assert(rhs >= 0, "TODO Negative power exponent needs MpQ return");
            __gmpz_pow_ui(y._ptr, _ptr, rhs);
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
        MpZ y = null;
        assert(rhs != 0, "Divison by zero");
        static if (isSigned!Integral)
        {
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                immutable ulong pos_rhs = -rhs; // make it positive
                return -cast(typeof(return))__gmpz_tdiv_r_ui(y._ptr, _ptr, pos_rhs);
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
        else
        {
            static assert(false);
        }
    }

    /// Returns: an unsigned type `lhs` divided by `this`.
    MpZ opBinaryRight(string s, Unsigned)(Unsigned lhs) const
        if ((s == "+" || s == "-" || s == "*" || s == "%") &&
            isUnsigned!Unsigned)
    {
        typeof(return) y = null;
        static      if (s == "+") { __gmpz_add_ui(y._ptr, _ptr, lhs); }// commutative
        else static if (s == "-") { __gmpz_ui_sub(y._ptr, lhs, _ptr); }
        else static if (s == "*") { __gmpz_mul_ui(y._ptr, _ptr, lhs); } // commutative
        else static if (s == "%") {
            assert(this != 0, "Divison by zero");
            __gmpz_tdiv_r(y._ptr, MpZ(lhs)._ptr, _ptr); // convert `lhs` to MpZ
        }
        else
        {
            static assert(false);
        }
        return y;
    }

    /// Returns: a signed type `lhs` divided by `this`.
    MpZ opBinaryRight(string s, Signed)(Signed lhs) const
        if ((s == "+" || s == "-" || s == "*" || s == "%") &&
            isSigned!Signed)
    {
        static if (s == "+" || s == "*")
        {
            return opBinary!s(lhs); // commutative reuse
        }
        else static if (s == "-")
        {
            typeof(return) y = null;
            if (lhs < 0)        // TODO handle `lhs == lhs.min`
            {
                immutable ulong pos_rhs = -lhs; // make it positive
                __gmpz_add_ui(y._ptr, _ptr, pos_rhs);
            }
            else
            {
                __gmpz_sub_ui(y._ptr, _ptr, lhs);
            }
            y.negate();
            return y;
        }
        else static if (s == "%")
        {
            typeof(return) y = null;
            assert(this != 0, "Divison by zero");
            __gmpz_tdiv_r(y._ptr, MpZ(lhs)._ptr, _ptr); // convert `lhs` to MpZ
            return y;
        }
        else
        {
            static assert(false);
        }
    }

    /// Dividend propagates quotient type to signed.
    Unqual!Integral opBinaryRight(string s, Integral)(Integral lhs) const
        if ((s == "/") &&
            isIntegral!Integral)
    {
        MpZ y = null;
        assert(this != 0, "Divison by zero");
        __gmpz_tdiv_q(y._ptr, MpZ(lhs)._ptr, _ptr);
        static      if (isSigned!Integral)
        {
            return cast(typeof(return))y;
        }
        else static if (isUnsigned!Integral)
        {
            import std.typecons : Signed;
            return cast(Signed!(typeof(return)))y;
        }
        else
        {
            static assert(false);
        }
    }

    /// Exponentation.
    MpZ opBinaryRight(string s, Integral)(Integral base) const
        if ((s == "^^") &&
            isIntegral!Integral)
    {
        static assert(false, "Convert `this MpZ` exponent to `ulong` and calculate power via static method `pow()`");
        // MpZ exp = null;
        // __gmpz_pow();
        // return exp;
    }

    ref MpZ opOpAssign(string s)(auto ref const MpZ rhs)
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%"))
    {
        static      if (s == "+") { __gmpz_add(_ptr, _ptr, rhs._ptr); }
        else static if (s == "-") { __gmpz_sub(_ptr, _ptr, rhs._ptr); }
        else static if (s == "*")
        {
            if (rhs == -1)
            {
                negate();
            }
            else
            {
                __gmpz_mul(_ptr, _ptr, rhs._ptr);
            }
        }
        else static if (s == "/")
        {
            assert(rhs != 0, "Divison by zero");
            if (rhs == -1)
            {
                negate();
            }
            else
            {
                __gmpz_tdiv_q(_ptr, _ptr, rhs._ptr);
            }
        }
        else static if (s == "%")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_r(_ptr, _ptr, rhs._ptr);
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    ref MpZ opOpAssign(string s, Unsigned)(Unsigned rhs)
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%" || s == "^^") &&
            isUnsigned!Unsigned)
    {
        static      if (s == "+") { __gmpz_add_ui(_ptr, _ptr, rhs); }
        else static if (s == "-") { __gmpz_sub_ui(_ptr, _ptr, rhs); }
        else static if (s == "*") { __gmpz_mul_ui(_ptr, _ptr, rhs); }
        else static if (s == "/") { assert(rhs != 0, "Divison by zero"); __gmpz_tdiv_q_ui(_ptr, _ptr, rhs); }
        else static if (s == "%") { assert(rhs != 0, "Divison by zero"); __gmpz_tdiv_r_ui(_ptr, _ptr, rhs); }
        else static if (s == "^^") { __gmpz_pow_ui(_ptr, _ptr, rhs); }
        else
        {
            static assert(false);
        }
        return this;
    }

    ref MpZ opOpAssign(string s, Signed)(Signed rhs)
        if ((s == "+" || s == "-" || s == "*" || s == "^^") &&
            isSigned!Signed)
    {
        static      if (s == "+")
        {
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                assert(rhs != rhs.min);
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_sub_ui(_ptr, _ptr, pos_rhs);
            }
            else
            {
                __gmpz_add_ui(_ptr, _ptr, rhs);
            }
        }
        else static if (s == "-")
        {
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                assert(rhs != rhs.min);
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_add_ui(_ptr, _ptr, pos_rhs);
            }
            else
            {
                __gmpz_sub_ui(_ptr, _ptr, rhs);
            }
        }
        else static if (s == "*")
        {
            if (rhs == -1)
            {
                negate();
            }
            else
            {
                __gmpz_mul_si(_ptr, _ptr, rhs);
            }
        }
        else static if (s == "^^")
        {
            assert(rhs >= 0, "Negative power exponent");
            __gmpz_pow_ui(_ptr, _ptr, rhs);
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    /// Returns: `this`.
    ref inout(MpZ) opUnary(string s)() inout
        if (s == "+")
    {
        return this;
    }

    /// Returns: negation of `this`.
    MpZ opUnary(string s)() const
        if (s == "-")
    {
        typeof(return) y = this.dup;
        y.negate();
        return y;
    }

    /// Negate `this` in-place.
    void negate()
    {
        _z._mp_size = -_z._mp_size; // fast C macro `mpz_neg` in gmp.h
    }

    /// Increase `this` by one.
    ref MpZ opUnary(string s)()
        if (s == "++")
    {
        __gmpz_add_ui(_ptr, _ptr, 1);
        return this;
    }

    /// Decrease `this` by one.
    ref MpZ opUnary(string s)()
        if (s == "--")
    {
        __gmpz_sub_ui(_ptr, _ptr, 1);
        return this;
    }

    /// Returns: `base` raised to the power of `exp`.
    static typeof(this) pow(IntegralBase,
                            IntegralExp)(IntegralBase base,
                                         IntegralExp exp)
        if (isIntegral!IntegralBase &&
            isIntegral!IntegralExp)
    {
        static if (isSigned!IntegralBase)
        {
            assert(base >= 0, "Negative power base");
            immutable ubase = cast(ulong)base;
        }
        else
        {
            immutable ubase = base;
        }

        static if (isSigned!IntegralExp)
        {
            assert(exp >= 0, "Negative power exponent");
            immutable uexp = cast(ulong)exp;
        }
        else
        {
            immutable uexp = exp;
        }

        typeof(return) y = null;
        __gmpz_ui_pow_ui(y._ptr, ubase, uexp);
        return y;
    }

    /// Returns: `this` ^^ `power` (mod `modulo`).
    MpZ powm()(auto ref const MpZ power,
               auto ref const MpZ modulo) const
    {
        typeof(return) rop = 0; // result
        __gmpz_powm(rop._ptr,
                    _ptr,
                    power._ptr,
                    modulo._ptr);
        return rop;
    }
    /// ditto
    MpZ powm()(ulong power,
               auto ref const MpZ modulo) const
    {
        typeof(return) rop = 0;       // result
        __gmpz_powm_ui(rop._ptr,
                       _ptr,
                       power,
                       modulo._ptr);
        return rop;
    }

    /// Returns: absolute value of `this`.
    MpZ!eval abs() const
    {
        typeof(return) y = null;
        __gmpz_abs(y._ptr, _ptr);
        return y;
    }

    /// Returns: number of digits in base `base`.
    size_t sizeInBase(int base) const
    {
        return __gmpz_sizeinbase(_ptr, base);
    }

    /// Check if `this` is zero.
    @property bool isZero() const
    {
        return _z._mp_size == 0; // fast
    }

    /// Check if `this` is odd. TODO use as specialcase in: this & 1
    @property bool isOdd() const
    {
        return (_z._mp_size != 0) & cast(int)(_z._mp_d[0]); // fast C macro `mpz_odd_p` in gmp.h
    }

    /// Check if `this` is odd.
    @property bool isEven() const
    {
        return !isOdd;            // fast C macro `mpz_even_p` in gmp.h
    }

    /// Check if `this` is negative. TODO use as specialcase in opCmp
    @property bool isNegative() const
    {
        return _z._mp_size < 0; // fast
    }

    /// Check if `this` is positive.
    @property bool isPositive() const
    {
        return !isNegative;     // fast
    }

    /** Returns: sign as either
        - -1 (`this` < 0),
        -  0 (`this` == 0), or
        - +1 (`this` > 0).
     */
    @property int sgn() const
    {
        return _z._mp_size < 0 ? -1 : _z._mp_size > 0; // fast C macro `mpz_sgn` in gmp.h
    }

    /// Number of significant `uint`s used for storing `this`.
    @property size_t uintLength() const
    {
        assert(false, "TODO use mpz_size");
    }

    /// Number of significant `ulong`s used for storing `this`.
    @property size_t uintLong() const
    {
        assert(false, "TODO use mpz_size");
    }

private:

    /// Type of limb in internal representation.
    alias Limb = __mp_limb_t;   // GNU MP alias

    /** Returns: limbs. */
    inout(Limb)[] _limbs() inout return @system // TODO scope
    {
        // import std.math : abs;
        return _z._mp_d[0 .. _limbCount];
    }

    /// Get number of limbs in internal representation.
    @property uint _limbCount() const
    {
        return _integralAbs(_z._mp_size);
    }

    /// @nogc-variant of `toStringz` with heap allocation of null-terminated C-string `stringz`.
    char* _allocStringzCopyOf(const string value) @nogc
    {
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
        return stringz;
    }

    /// Returns: pointer to internal GMP C struct.
    inout(__mpz_struct)* _ptr() inout return // TODO scope
    {
        return &_z;
    }

    __mpz_struct _z;            // internal libgmp C struct

    // qualified C memory managment
    static @safe pure nothrow @nogc
    {
        pragma(mangle, "malloc") void* qualifiedMalloc(size_t size);
        pragma(mangle, "free") void qualifiedFree(void* ptr);
    }
}

pure nothrow pragma(inline, true):

/** Instantiator for `MpZ`. */
MpZ!eval mpz(Eval eval = Eval.direct, Args...)(Args args) @safe @nogc
{
    return typeof(return)(args);
}

/// Swap contents of `x` with contents of `y`.
void swap(Eval evalX, Eval evalY)(ref MpZ!evalX x,
                                  ref MpZ!evalY y) @nogc
{
    import std.algorithm.mutation : swap;
    swap(x, y); // x.swap(y);
}

string toDecimalString(Eval eval)(auto ref const MpZ!eval x) // for `std.bigint.BigInt` compatibility
{
    return x.toString(10);
}

string toHex(Eval eval)(auto ref const MpZ!eval x) // for `std.bigint.BigInt` compatibility
{
    return x.toString(16, true);
}

/// Returns: the absolute value of `x` converted to the corresponding unsigned type.
Unsigned!T absUnsign(T, Eval eval)(auto ref const MpZ!eval x) // for `std.bigint.BigInt` compatibility
    if (isIntegral!T)
{
    return _integralAbs(cast(T)x);
}

/// Returns: absolute value of `x`.
MpZ!eval abs(Eval eval)(const ref MpZ!eval x) @trusted @nogc
{
    typeof(return) y = null;
    __gmpz_abs(y._ptr, x._ptr);
    return y;
}

/// convert to string
@safe unittest
{
    assert(mpz(    42).toString ==   `42`);
    assert(mpz(   -42).toString ==  `-42`);
    assert(mpz(`-101`).toString == `-101`);

    assert(mpz(-42).toDecimalString == `-42`);
    assert(mpz( 42).toDecimalString ==  `42`);

    assert(mpz( 0).toHex == `0`);
    assert(mpz( 1).toHex == `1`);
    assert(mpz( 9).toHex == `9`);
    assert(mpz(10).toHex == `A`);
    assert(mpz(14).toHex == `E`);
    assert(mpz(15).toHex == `F`);
    assert(mpz(16).toHex == `10`);

    assert(mpz(-42).absUnsign!ulong == 42);
    assert(mpz( 42).absUnsign!ulong == 42);
}

/// as hash table key
@safe unittest
{
    // TODO disabled until non-copyable types work in AA's
    // string[Z] aa;
    // aa[mpz(123)] = "abc";
    // aa[mpz(456)] = "def";
    // assert(aa[mpz(123)] == "abc");
    // assert(aa[mpz(456)] == "def");
}

///
@safe @nogc unittest
{
    const _ = mpz(cast(uint)42);
    const a = mpz(42);
    const b = mpz(43UL);
    const c = mpz(43.0);

    // eval cast

    assert(a);
    assert(cast(ulong)a == a);
    assert(cast(ulong)a == 42);
    assert(cast(long)a == a);
    assert(cast(long)a == 42);
    assert(cast(double)a == 42.0);

    // binary

    assert(mpz(`0b11`) == 3);
    assert(mpz(`0B11`) == 3);

    // octal

    assert(mpz(`07`) == 7);
    assert(mpz(`010`) == 8);

    // hexadecimal

    assert(mpz(`0x10`) == 16);
    assert(mpz(`0X10`) == 16);

    // decimal

    assert(mpz(`101`) == 101);
    assert(mpz(`101`, 10) == 101);

    immutable ic = mpz(101UL);

    assert(a == a.dup);
    assert(ic == ic.dup);

    // equality

    assert(a == a);
    assert(a == mpz(42));
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
    assert(a < mpz(43));
    assert(a < 43);
    assert(a < cast(uint)43);
    assert(a < 43UL);
    assert(a < 43.0);

    // greater than

    assert(b > a);
    assert(b > mpz(42));
    assert(b > 42);
    assert(b > cast(uint)42);
    assert(b > 42UL);
    assert(b > 42.0);

    // absolute value

    assert(abs(a) == a);        // free function
    assert(a.abs == a);         // member

    // negated value

    assert(-a == -42);
    assert(-(-a) == a);

    // addition

    assert(a + b == b + a);     // commutative
    assert(a + mpz(43) == b + a);
    assert(a + 0 == a);
    assert(a + 1 != a);
    assert(0 + a == a);
    assert(1 + a != a);
    assert(a + 0UL == a);
    assert(a + 1UL != a);
    assert(a + b == 42 + 43);
    assert(1 + a == 43);
    assert(a + (-1) == 41);
    assert(1UL + a == 43);
    assert(a + 1 == 1 + a);       // commutative
    assert(a + (-1) == (-1) + a); // commutative
    assert(1UL + a == a + 1UL);   // commutative

    // subtraction

    assert(a - 2 == 40);
    assert(2 - a == -40);
    assert(-2 - a == -44);
    assert(a - 2 == -(2 - a));     // commutative
    assert(a - (-2) == 44);
    assert(44UL - mpz(42) == 2);

    // multiplication

    assert(a * 1UL == a);
    assert(a * 1 == a);
    assert(1 * a == a);
    assert(1UL * a == a);
    assert((-1) * a == -a);
    assert(a * 2 != a);
    assert(a * -2 == -(2*a));
    assert(a * b == b * a);
    assert(a * b == 42UL * 43UL);

    // division

    assert(mpz(27) / mpz(3) == 9);
    assert(mpz(27) /   3  == 9);

    assert(mpz(27) / mpz(10)  == 2);
    assert(mpz(27) /   10   == 2);
    assert(mpz(27) /   10UL == 2);

    assert(mpz(27) / -3   == -9);
    assert(mpz(27) /  3UL ==  9);

    assert(mpz(27) / -10   == -2);

    assert(28   / mpz( 3) ==  9);
    assert(28UL / mpz( 3) ==  9);

    assert(28   / mpz(-3) == -9);
    assert(28UL / mpz(-3) == -9);

    // modulo/remainder

    assert(mpz(27) % mpz(3) == 0);
    assert(mpz(27) % mpz(10) == 7);

    assert(mpz(27) % 3 == 0);
    assert(mpz(-27) % 3 == 0);

    assert(mpz(27) % 10 == 7);
    assert(mpz(27) % 10 == 7);

    assert(28   % mpz(3) == 1);
    assert(28UL % mpz(3) == 1);

    assert(mpz( 28)  % -3 == -1); // negative divisor gives negative remainder according to https://en.wikipedia.org/wiki/Remainder
    assert(mpz(-28)  %  3 == 1);  // dividend sign doesn't affect remainder

    //
    assert(mpz( 28)  % mpz(-3) == 1);  // TODO should be -1
    assert(mpz(-28)  % mpz( 3) == -1);  // TODO should be 1
    assert( 28  % mpz(-3) == 1);      // TODO should be -1
    assert(-28  % mpz( 3) == -1);     // TODO should be 1

    // modulo/remainder

    immutable one = mpz(1);
    const two = mpz(2);
    immutable three = mpz(3);
    const four = mpz(4);
    immutable five = mpz(5);
    const six = mpz(6);
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
    assert(1 - six == -5);
    assert(1L - six == -5);
    assert(1UL - six == -5);

    // exponentiation

    assert(mpz(0)^^0 == 1);
    assert(mpz(3)^^3 == 27);
    assert(mpz(3)^^3L == 27);
    assert(mpz(2)^^8 == 256);
    assert(mpz(2)^^8L == 256);
    assert(mpz(2)^^8UL == 256);

    assert(Z.pow(2UL, 8UL) == 256);
    assert(Z.pow(2UL, 8) == 256);
    assert(Z.pow(2UL, 8) == 256);
    assert(Z.pow(2, 8) == 256);

    // disallow power exponent to be an `MpZ`
    assert(!__traits(compiles, 2^^mpz(8) == 256));
    assert(!__traits(compiles, 2L^^mpz(8) == 256));
    assert(!__traits(compiles, 2UL^^mpz(8) == 256));

    // exponentiation plus modulus

    assert(mpz(2).powm(mpz(8), mpz(8)) == mpz(0));
    assert(mpz(2).powm(mpz(3), mpz(16)) == mpz(8));
    assert(mpz(3).powm(mpz(3), mpz(16)) == mpz(11));

    assert(mpz(2).powm(8, mpz(8)) == mpz(0));
    assert(mpz(2).powm(3, mpz(16)) == mpz(8));
    assert(mpz(3).powm(3, mpz(16)) == mpz(11));

    // swap

    auto x = mpz(42);
    auto y = mpz(43);

    assert(x == 42);
    assert(y == 43);

    x.swap(y);

    assert(y == 42);
    assert(x == 43);

    swap(x, y);

    assert(x == 42);
    assert(y == 43);

    assert(mpz(null).fromString("42") == 42);
    assert(mpz(null).fromString("11", 2) == 3);
    assert(mpz(null).fromString("7", 8) == 7);
    assert(mpz(null).fromString("7") == 7);
    assert(mpz(null).fromString("e", 16) == 14);
    assert(mpz(null).fromString("f", 16) == 15);
    assert(mpz(null).fromString("0xe") == 14);
    assert(mpz(null).fromString("0xf") == 15);
    assert(mpz(null).fromString("10", 16) == 16);
    assert(mpz(null).fromString("10", 32) == 32);

    // odd and even

    assert(mpz(0).isEven);
    assert(mpz(1).isOdd);
    assert(mpz(2).isEven);
    assert(mpz(3).isOdd);

    assert(mpz(-1).isOdd);
    assert(mpz(-2).isEven);
    assert(mpz(-3).isOdd);

    assert(mpz("300000000000000000000000000000000000000").isEven);
    assert(mpz("300000000000000000000000000000000000001").isOdd);
    assert(mpz("300000000000000000000000000000000000002").isEven);
    assert(mpz("300000000000000000000000000000000000003").isOdd);

    // negative and positive

    assert(mpz(0).isPositive);
    assert(mpz(1).isPositive);
    assert(mpz(2).isPositive);
    assert(mpz(3).isPositive);

    assert(mpz(-1).isNegative);
    assert(mpz(-2).isNegative);
    assert(mpz(-3).isNegative);

    // sign function (sgn)

    assert(mpz(long.min).sgn == -1);
    assert(mpz(int.min).sgn  == -1);
    assert(mpz(-2).sgn == -1);
    assert(mpz(-1).sgn == -1);
    assert(mpz( 0).sgn ==  0);
    assert(mpz( 1).sgn ==  1);
    assert(mpz( 2).sgn ==  1);
    assert(mpz(int.max).sgn  == 1);
    assert(mpz(long.max).sgn == 1);

    assert(!mpz(long.min).isZero);
    assert(!mpz(int.min).isZero);
    assert(!mpz(-2).isZero);
    assert(!mpz(-1).isZero);
    assert( mpz( 0).isZero);
    assert(!mpz( 1).isZero);
    assert(!mpz( 2).isZero);
    assert(!mpz(int.max).isZero);
    assert(!mpz(long.max).isZero);

    // internal limb count

    assert(mpz(0)._limbCount == 0);
    assert(mpz(1)._limbCount == 1);
    assert(mpz(2)._limbCount == 1);

    assert(Z.pow(2UL, 32UL)._limbCount == 1);

    assert(Z.pow(2UL, 63UL)._limbCount == 1);
    assert(Z.pow(2UL, 63UL + 1)._limbCount == 2);

    assert(Z.pow(2UL, 127UL)._limbCount == 2);
    assert(Z.pow(2UL, 127UL + 1)._limbCount == 3);

    assert(Z.pow(2UL, 255UL)._limbCount == 4);
    assert(Z.pow(2UL, 255UL + 1)._limbCount == 5);
}

/// generators
@safe @nogc unittest
{
    assert(Z.mersennePrime(15) == 2^^15 - 1);
    assert(Z.mersennePrime(15UL) == 2^^15 - 1);
}

/// Phobos unittests
version(unittestPhobos) @safe @nogc unittest
{
    alias bigInt = mpz;
    alias BigInt = Z;     // Phobos naming convention

    {
        const BigInt a = "9588669891916142";
        const BigInt b = "7452469135154800";
        const c = a * b;
        assert(c == BigInt("71459266416693160362545788781600"));
        auto d = b * a;
        assert(d == BigInt("71459266416693160362545788781600"));
        assert(d == c);
        d = c * BigInt("794628672112");
        assert(d == BigInt("56783581982794522489042432639320434378739200"));
        auto e = c + d;
        assert(e == BigInt("56783581982865981755459125799682980167520800"));
        const f = d + c;
        assert(f == e);
        auto g = f - c;
        assert(g == d);
        g = f - d;
        assert(g == c);
        e = 12_345_678;
        g = c + e;
        immutable h = g / b;
        const i = g % b;
        assert(h == a);
        assert(i == e);
        BigInt j = "-0x9A56_57f4_7B83_AB78";
        j ^^= 11UL;
        j ^^= 2L;
        j ^^= 2;
    }

    {
        auto b = BigInt("1_000_000_000");

        b += 12_345;
        assert(b == 1_000_012_345);
        b += -12_345;
        assert(b == 1_000_000_000);

        b -= -12_345;
        assert(b == 1_000_012_345);
        b -= +12_345;
        assert(b == 1_000_000_000);

        b += 12_345;
        assert(b == 1_000_012_345);

        b /= 5UL;
        assert(b == 200_002_469);
    }

    {
        auto x = BigInt("123");
        const y = BigInt("321");
        x += y;
        assert(x == 444);
    }

    {
        const x = BigInt("123");
        const y = BigInt("456");
        const BigInt z = x * y;
        assert(z == 56_088);
    }

    {
        auto x = BigInt("123");
        x *= 300;
        assert(x == 36_900);
    }

    {
        auto x = BigInt("123");
        x *= -1;
        assert(x == -123);
    }

    {
        const x  = BigInt("1_000_000_500");

        immutable ulong ul  = 2_000_000UL;
        immutable uint ui   = 500_000;
        immutable ushort us = 30_000;
        immutable ubyte ub  = 50;

        immutable long  l = 1_000_000L;
        immutable int   i = 500_000;
        immutable short s = 30_000;
        immutable byte b  = 50;

        static assert(is(typeof(x % ul)  == ulong));
        static assert(is(typeof(x % ui)  == uint));
        static assert(is(typeof(x % us)  == ushort));
        static assert(is(typeof(x % ub)  == ubyte));

        static assert(is(typeof(x % l)  == long));
        static assert(is(typeof(x % i)  == int));
        static assert(is(typeof(x % s)  == short));
        static assert(is(typeof(x % b)  == byte));

        assert(x % ul == 500);
        assert(x % ui == 500);
        assert(x % us  == 10_500);
        assert(x % ub == 0);

        assert(x % l  == 500L);
        assert(x % i  == 500);
        assert(x % s  == 10_500);
        assert(x % b == 0);
    }

    {
        const x = BigInt("100");
        const BigInt y = 123 + x;
        assert(y == BigInt("223"));

        const BigInt z = 123 - x;
        assert(z == BigInt("23"));

        // Dividing a built-in integer type by BigInt always results in
        // something that fits in a built-in type, so the built-in type is
        // returned, not BigInt.
        static assert(is(typeof(1000 / x) == int));
        assert(1000 / x == 10);
    }

    {
        auto x = BigInt("1234");
        assert(+x  == BigInt(" 1234"));
        assert(-x  == BigInt("-1234"));
        ++x;
        assert(x == BigInt("1235"));
        --x;
        assert(x == BigInt("1234"));
    }

    {
        const x = BigInt("12345");
        const y = BigInt("12340");
        immutable int z = 12345;
        immutable int w = 54321;
        assert(x == x);
        assert(x != y);
        assert(x == y + 5);
        assert(x == z);
        assert(x != w);
    }

    {
        // non-zero values are regarded as `true`
        const x = BigInt("1");
        const y = BigInt("10");
        assert(x);
        assert(y);

        // zero value is regarded as `false`
        const z = BigInt("0");
        assert(!z);
    }

    {
        assert(cast(int)BigInt("0") == 0);
        assert(cast(ubyte)BigInt("0") == 0);

        assert(cast(ubyte)BigInt(255) == 255);
        assert(cast(ushort)BigInt(65535) == 65535);
        assert(cast(uint)BigInt(uint.max) == uint.max);
        assert(cast(ulong)BigInt(ulong.max) == ulong.max);

        assert(cast(byte)BigInt(-128) == -128);
        assert(cast(short)BigInt(-32768) == -32768);
        assert(cast(int)BigInt(int.min) == int.min);
        assert(cast(long)BigInt(long.min) == long.min);

        assert(cast(byte)BigInt(127) == 127);
        assert(cast(short)BigInt(32767) == 32767);
        assert(cast(int)BigInt(int.max) == int.max);
        assert(cast(long)BigInt(long.max) == long.max);

        // TODO:
        // import std.conv : to, ConvOverflowException;
        // import std.exception : assertThrown;
        // assertThrown!ConvOverflowException(BigInt("256").to!ubyte);
        // assertThrown!ConvOverflowException(BigInt("-1").to!ubyte);
    }

    {
        // TODO
        // segfaults because with double free
        // const(BigInt) x = BigInt("123");
        // BigInt y = cast()x;    // cast away const
        // assert(y == x);
    }

    {
        auto x = BigInt("100");
        auto y = BigInt("10");
        int z = 50;
        const int w = 200;
        assert(y < x);
        assert(x > z);
        assert(z > y);
        assert(x < w);
    }

    {
        assert(BigInt("12345").toLong() == 12_345);
        assert(BigInt("-123450000000000000000000000000").toLong() == long.min);
        assert(BigInt("12345000000000000000000000000000").toLong() == long.max);
    }

    {
        assert(BigInt("12345").toInt() == 12_345);
        assert(BigInt("-123450000000000000000000000000").toInt() == int.min);
        assert(BigInt("12345000000000000000000000000000").toInt() == int.max);
    }
}

// Fermats Little Theorem
pure unittest
{
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
                const p = Z.mersennePrime(i); // power
                const a = Z(j); // base
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
        Z r1 = 0;
    outermost:
        foreach (immutable ulong a; 1 .. LIMIT)
        {
            foreach (immutable ulong b; a .. LIMIT)
            {
                foreach (immutable ulong c; b .. LIMIT)
                {
                    foreach (immutable ulong d; c .. LIMIT)
                    {
                        r1 = ((Z(a) ^^ POWER) +
                              (Z(b) ^^ POWER) +
                              (Z(c) ^^ POWER) +
                              (Z(d) ^^ POWER));
                        Z rem = 0;
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

// utility
T _integralAbs(T)(T x)
    if (isIntegral!T)
{
    return x>=0 ? x : -x;
}

// faster traits. TODO move to std.traits
enum isArithmetic(T) = __traits(isArithmetic, T);
enum isFloating(T) = __traits(isFloating, T);
enum isFloatingPoint(T) = __traits(isFloating, T);
enum isScalar(T) = __traits(isScalar, T);
enum isScalarType(T) = __traits(isScalar, T);
enum isIntegral(T) = __traits(isIntegral, T);
enum isUnsigned(T) = __traits(isUnsigned, T);
enum isSigned(T) = __traits(isArithmetic, T) && !__traits(isUnsigned, T);
enum isStaticArray(T) = __traits(isStaticArray, T);
enum isAssociativeArray(T) = __traits(isAssociativeArray, T);
enum isAbstractClass(T) = __traits(isAbstractClass, T);
enum isFinalClass(T) = __traits(isFinalClass, T);
enum isPOD(T) = __traits(isPOD, T);
enum isNested(T) = __traits(isNested, T);
enum isVirtualFunction(alias fn) = __traits(isVirtualFunction, fn);
enum isVirtualMethod(alias m) = __traits(isVirtualMethod, m);
enum isAbstractFunction(alias fn) = __traits(isAbstractFunction, fn);
enum isFinalFunction(alias fn) = __traits(isFinalFunction, fn);
enum isOverrideFunction(alias fn) = __traits(isOverrideFunction, fn);
enum isStaticFunction(alias fn) = __traits(isStaticFunction, fn);
enum isRef(alias fn) = __traits(isRef, fn);
enum isOut(alias fn) = __traits(isOut, fn);
enum isLazy(alias fn) = __traits(isLazy, fn);
enum isTemplate(alias sym) = __traits(isTemplate, sym);
enum hasMember(T, string member) = __traits(hasMember, T, member);
enum IdentifierStringOfSymbol(alias sym) = __traits(identifier, sym);

version(unittest)
{
    // import dbgio;
    alias Z = MpZ!(Eval.direct);
}

// C API
extern(C)
{
    alias __mp_limb_t = ulong;    // see `mp_limb_t` gmp.h. TODO detect when it is `uint` instead
    struct __mpz_struct
    {
        int _mp_alloc;		/* Number of *limbs* allocated and pointed to by
                                   the _mp_d field.  */
        int _mp_size;           /* abs(_mp_size) is the number of limbs the last
                                   field points to.  If _mp_size is negative
                                   this is a negative number.  */
        __mp_limb_t* _mp_d;       /* Pointer to the limbs. */
    }
    static assert(__mpz_struct.sizeof == 16); // fits in two 64-bit words

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

    void __gmpz_set (mpz_ptr rop, mpz_srcptr op);
    void __gmpz_set_ui (mpz_ptr rop, ulong op);
    void __gmpz_set_si (mpz_ptr rop, long op);
    void __gmpz_set_d (mpz_ptr rop, double op);
    int __gmpz_set_str (mpz_ptr, const char*, int);

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

    void __gmpz_tdiv_q (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_tdiv_r (mpz_ptr, mpz_srcptr, mpz_srcptr);
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
    long __gmpz_get_si (mpz_srcptr);
    double __gmpz_get_d (mpz_srcptr);

    // TODO wrap:
    void __gmpz_root (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_rootrem (mpz_ptr, mpz_ptr, mpz_srcptr, ulong);

    void __gmpz_sqrt (mpz_ptr, mpz_srcptr);
    void __gmpz_sqrtrem (mpz_ptr, mpz_ptr, mpz_srcptr);

    void __gmpz_perfect_power_p (mpz_ptr, mpz_srcptr);
}

// link with C library GNU MP
pragma(lib, "gmp");
