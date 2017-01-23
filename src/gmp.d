/** High-level wrapper for GNU Multiple Precision (MP) library.

    TODO _ccc Make _ccc two => uint _initCCC and uint _opCCC, mutatingCallCount
    return their sum. Use these counters to make more precise asserts on how
    assignments and operators are lowered to calls to C __gmpz-calls

    TODO disallow construction and assignment from floating point? Check with
    other GMP interfaces and std.bigint.

    TODO Should `in` operator be used for anything good?

    TODO Support operators
          <<      >>      >>>
          &= |= ^=
*/
module gmp;

debug import core.stdc.stdio : printf;
import std.typecons : Unsigned, Unqual;
import std.meta : AliasSeq;
import std.traits : isInstanceOf;
import std.algorithm.mutation : move, moveEmplace;

/// Call unittests taking long to execute.
enum unittestLong = false;

version(unittest)
{
    version = ccc;              // do C mutation call count
}

/** Is `true` iff `T` is a GNU MP arithmetic type (`long`, `ulong` or `double`). */
enum isGMPArithmetic(T) = is(T == long) && is(T == ulong) && is(T == double);

/// Is `true` if type `T` can be evaluated to a `MpZ` value.
enum isMpZExpr(T) = (is(Unqual!(typeof(T.eval())) == MpZ)); // which returns an `MpZ`
/// Is `true` if type `T` is lazy (not yet evaluated) `MpZ`-expression.
enum isLazyMpZExpr(T) = (!is(Unqual!T == MpZ) &&            // exclude direct value
                         isMpZExpr!T);

version(unittest)
{
    static assert(!isMpZExpr!int);
}

// TODO use these imports instead of the ones below
// import deimos.gmp.gmp;
// import deimos.gmp.integer;

/** Arbitrary (multi) precision signed integer (Z).
    Wrapper for GNU MP (GMP)'s type `mpz_t` and functions `mpz_.*`.
 */
struct MpZ
{
    /// Default conversion base.
    private enum defaultBase = 10;

    pure nothrow pragma(inline, true):

    /// Convert to `string` in base `base`.
    string toString(uint base = defaultBase, bool upperCaseDigits = false) const @trusted
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
    this(typeof(null)) @trusted
    {
        initialize();             // TODO is there a faster way?
        assert(this == MpZ.init); // if this is same as default
    }

    /// Construct from expression `expr`.
    this(Expr)(Expr expr)
        if (isLazyMpZExpr!Expr)
    {
        // TODO ok to just assume zero-initialized contents at `_z` before...
        this = expr;            // ...calling this.opAssign ?x
    }

    /** Construct from `value`. */
    this(T)(T value) @trusted
        if (isArithmetic!T)
    {
        version(ccc) ++_ccc;
        static      if (isUnsigned!T)
            __gmpz_init_set_ui(_ptr, value);
        else static if (isFloating!T)
            __gmpz_init_set_d(_ptr, value);
        else                    // isSigned integral
            __gmpz_init_set_si(_ptr, value);
    }

    /** Construct from `value` in base `base`.
        If `base` is 0 it's guessed from contents of `value`.
        */
    pragma(inline, false)
    this(in string value, uint base = 0) @trusted // TODO Use Optional/Nullable when value is nan, or inf
    {
        assert(base == 0 || (base >= 2 && base <= 62));
        char* stringz = _allocStringzCopyOf(value);
        immutable int status = __gmpz_init_set_str(_ptr, stringz, base); version(ccc) ++_ccc;
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
        this(ref const MpZ value) @trusted
        {
            __gmpz_init_set(_ptr, value._ptr); version(ccc) ++_ccc;
        }
        /// Construct copy of `value`.
        this(MpZ value) @trusted
        {
            moveEmplace(value, this); // fast
        }
    }
    else
    {
        /// Disable copy construction.
        @disable this(this);
    }

    /** Initialize internal struct. */
    private void initialize() @trusted // cannot be called `init` as that will override builtin type property
    {
        __gmpz_init(_ptr); version(ccc) ++_ccc;
    }

    /// Swap content of `this` with `rhs`.
    void swap(ref MpZ rhs) @safe
    {
        import std.algorithm.mutation : swap;
        swap(this, rhs); // faster than __gmpz_swap(_ptr, rhs._ptr); version(ccc) ++_ccc;
    }

    /// Returns: (duplicate) copy of `this`.
    MpZ dup() const @trusted
    {
        typeof(return) y = void;
        __gmpz_init_set(y._ptr, _ptr); ++y._ccc;
        return y;
    }

    /// Assign from `rhs`.
    ref MpZ opAssign()(auto ref const MpZ rhs) return @trusted // TODO scope
    {
        __gmpz_set(_ptr, rhs._ptr); version(ccc) ++_ccc;
        return this;
    }
    /// ditto
    ref MpZ opAssign(Expr)(auto ref Expr rhs) return @trusted // TODO scope
        if (isLazyMpZExpr!Expr)
    {
        static      if (isInstanceOf!(MpzAddExpr, Expr))
        {
            __gmpz_add(this._ptr,
                       rhs.e1.eval()._ptr,
                       rhs.e2.eval()._ptr); version(ccc) ++_ccc;
        }
        else static if (isInstanceOf!(MpzSubExpr, Expr))
        {
            __gmpz_sub(this._ptr,
                       rhs.e1.eval()._ptr,
                       rhs.e2.eval()._ptr); version(ccc) ++_ccc;
        }
        else static if (isInstanceOf!(MpzMulExpr, Expr))
        {
            __gmpz_mul(this._ptr,
                       rhs.e1.eval()._ptr,
                       rhs.e2.eval()._ptr); version(ccc) ++_ccc;
        }
        else static if (isInstanceOf!(MpzDivExpr, Expr))
        {
            __gmpz_tdiv_q(this._ptr,
                          rhs.e1.eval()._ptr,
                          rhs.e2.eval()._ptr); version(ccc) ++_ccc;
        }
        else static if (isInstanceOf!(MpzModExpr, Expr))
        {
            __gmpz_tdiv_r(this._ptr,
                          rhs.e1.eval()._ptr,
                          rhs.e2.eval()._ptr); version(ccc) ++_ccc;
        }
        else static if (isInstanceOf!(MpzNegExpr, Expr))
        {
            __gmpz_neg(this._ptr,
                       rhs.e1.eval()._ptr); version(ccc) ++_ccc;
        }
        else
        {
            this = rhs.eval();     // evaluate and move
        }
        return this;
    }
    /// ditto
    ref MpZ opAssign(T)(T rhs) return @trusted // TODO scope
        if (isArithmetic!T)
    {
        static if      (isUnsigned!T)
        {
            __gmpz_set_ui(_ptr, rhs);
        }
        else static if (isFloating!T)
        {
            __gmpz_set_d(_ptr, rhs);
        }
        else static if (isSigned!T)
        {
            pragma(msg, T);
            __gmpz_set_si(_ptr, rhs);
        }
        else
        {
            static assert(false);
        }
        version(ccc) ++_ccc;
        return this;
    }

    /** Assign `this` from `string` `rhs` interpreted in base `base`.
        If `base` is 0 it's guessed from contents of `value`.
    */
    ref MpZ fromString(in string rhs, uint base = 0) return @trusted // TODO scope
    {
        assert(base == 0 || (base >= 2 && base <= 62));
        char* stringz = _allocStringzCopyOf(rhs);
        immutable int status = __gmpz_set_str(_ptr, stringz, base); version(ccc) ++_ccc;
        qualifiedFree(stringz);
        assert(status == 0, "Parameter `rhs` does not contain an integer");
        return this;
    }

    /// Destruct `this`.
    ~this() @trusted
    {
        if (_ptr)
        {
            __gmpz_clear(_ptr); version(ccc) ++_ccc;
        }
    }

    /// Returns: `true` iff `this` equals `rhs`.
    bool opEquals()(auto ref const MpZ rhs) const @trusted // TODO scope
    {
        if (_ptr == rhs._ptr)   // fast equality
        {
            return true;        // fast bailout
        }
        return __gmpz_cmp(_ptr, rhs._ptr) == 0;
    }
    /// ditto
    bool opEquals(Rhs)(Rhs rhs) const @trusted // TODO scope
        if (isArithmetic!Rhs)
    {
        if (rhs == 0)
        {
            return isZero;      // optimization
        }
        static      if (isUnsigned!Rhs)
        {
            return __gmpz_cmp_ui(_ptr, cast(ulong)rhs) == 0;
        }
        else static if (isFloating!Rhs)
        {
            return __gmpz_cmp_d(_ptr, cast(double)rhs) == 0; // TODO correct to do this cast here?
        }
        else                    // isSigned integral
        {
            return __gmpz_cmp_si(_ptr, cast(long)rhs) == 0;
        }
    }

    /// Compare `this` to `rhs`.
    int opCmp()(auto ref const MpZ rhs) const @trusted // TODO scope
    {
        if (rhs == 0)
        {
            return sgn();       // optimization
        }
        return __gmpz_cmp(_ptr, rhs._ptr);
    }

    /// ditto
    int opCmp(T)(T rhs) const @trusted // TODO scope
        if (isArithmetic!T)
    {
        if (rhs == 0)
        {
            return sgn();       // optimization
        }
        static      if (isUnsigned!T)
        {
            return __gmpz_cmp_ui(_ptr, rhs);
        }
        else static if (isFloating!T)
        {
            return __gmpz_cmp_d(_ptr, rhs);
        }
        else                    // isSigned integral
        {
            return __gmpz_cmp_si(_ptr, rhs);
        }
    }

    /// Cast to `bool`.
    bool opCast(T : bool)() const // TODO scope
    {
        return !isZero;
    }

    /// Cast to arithmetic type `T`.
    T opCast(T)() const @trusted // TODO scope
        if (isArithmetic!T)
    {
        static      if (isUnsigned!T)
        {
            return cast(T)__gmpz_get_ui(_ptr);
        }
        else static if (isFloating!T)
        {
            return cast(T)__gmpz_get_d(_ptr);
        }
        else                    // isSigned integral
        {
            return cast(T)__gmpz_get_si(_ptr);
        }
    }

    /** Returns: The value of this as a `long`, or +/- `long.max` if outside
        the representable range.
    */
    long toLong() const @trusted
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
    int toInt() const @trusted
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

    /** Returns: `this` `s` `rhs`. */
    MpZ opBinary(string s)(auto ref const MpZ rhs) const @trusted // direct value
        if ((s == "+" || s == "-" ||
             s == "*" || s == "/" || s == "%" ||
             s == "&" || s == "|" || s == "^"))
    {
        static if (!__traits(isRef, rhs)) // r-value `rhs`
        {
            MpZ* mut_rhs = (cast(MpZ*)(&rhs)); // @trusted because `MpZ` has no aliased indirections
            static      if (s == "+")
            {
                __gmpz_add(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else static if (s == "-")
            {
                __gmpz_sub(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else static if (s == "*")
            {
                __gmpz_mul(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else static if (s == "/")
            {
                assert(rhs != 0, "Divison by zero");
                __gmpz_tdiv_q(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else static if (s == "%")
            {
                assert(rhs != 0, "Divison by zero");
                __gmpz_tdiv_r(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else static if (s == "&")
            {
                __gmpz_and(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else static if (s == "|")
            {
                __gmpz_ior(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else static if (s == "^")
            {
                __gmpz_xor(mut_rhs._ptr, _ptr, rhs._ptr); version(ccc) ++mut_rhs._ccc;
            }
            else
            {
                static assert(false);
            }
            return move(*mut_rhs); // TODO shouldn't have to call `move` here
        }
        else
        {
            typeof(return) y = null;
            static      if (s == "+")
            {
                __gmpz_add(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else static if (s == "-")
            {
                __gmpz_sub(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else static if (s == "*")
            {
                __gmpz_mul(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else static if (s == "/")
            {
                assert(rhs != 0, "Divison by zero");
                __gmpz_tdiv_q(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else static if (s == "%")
            {
                assert(rhs != 0, "Divison by zero");
                __gmpz_tdiv_r(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else static if (s == "&")
            {
                __gmpz_and(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else static if (s == "|")
            {
                __gmpz_ior(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else static if (s == "^")
            {
                __gmpz_xor(y._ptr, _ptr, rhs._ptr); version(ccc) ++y._ccc;
            }
            else
            {
                static assert(false);
            }
            return y;
        }
    }

    /// ditto
    MpZ opBinary(string s, Rhs)(auto ref const Rhs rhs) const
        if (isLazyMpZExpr!Rhs && // lazy value
            (s == "+" || s == "-" || s == "*" || s == "/" || s == "%"))
    {
        static assert(false, "TODO");
    }

    MpZ opBinary(string s, Rhs)(Rhs rhs) const @trusted
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "^^") &&
            isUnsigned!Rhs)
    {
        typeof(return) y = null;
        version(ccc) ++y._ccc;
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
            assert(rhs != 0, "Divison by zero");
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

    MpZ opBinary(string s, Rhs)(Rhs rhs) const @trusted
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "^^") &&
            isSigned!Rhs)
    {
        typeof(return) y = null;
        version(ccc) ++y._ccc;
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
    Unqual!Rhs opBinary(string s, Rhs)(Rhs rhs) const @trusted
        if ((s == "%") &&
            isIntegral!Rhs)
    {
        assert(rhs != 0, "Divison by zero");
        MpZ y = null;
        version(ccc) ++y._ccc;
        static if (isSigned!Rhs)
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
        else static if (isUnsigned!Rhs)
        {
            return cast(typeof(return))__gmpz_tdiv_r_ui(y._ptr, _ptr, rhs);
        }
        else
        {
            static assert(false);
        }
    }

    /// Returns: an unsigned type `lhs` divided by `this`.
    MpZ opBinaryRight(string s, Lhs)(Lhs lhs) const @trusted
        if ((s == "+" || s == "-" || s == "*" || s == "%") &&
            isUnsigned!Lhs)
    {
        typeof(return) y = null;
        version(ccc) ++y._ccc;
        static      if (s == "+")
        {
            __gmpz_add_ui(y._ptr, _ptr, lhs); // commutative
        }
        else static if (s == "-")
        {
            __gmpz_ui_sub(y._ptr, lhs, _ptr);
        }
        else static if (s == "*")
        {
            __gmpz_mul_ui(y._ptr, _ptr, lhs); // commutative
        }
        else static if (s == "%")
        {
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
    MpZ opBinaryRight(string s, Lhs)(Lhs lhs) const @trusted
        if ((s == "+" || s == "-" || s == "*" || s == "%") &&
            isSigned!Lhs)
    {
        static if (s == "+" || s == "*")
        {
            return opBinary!s(lhs); // commutative reuse
        }
        else static if (s == "-")
        {
            typeof(return) y = null;
            version(ccc) ++y._ccc;
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
            version(ccc) ++y._ccc;
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
    Unqual!Lhs opBinaryRight(string s, Lhs)(Lhs lhs) const @trusted
        if ((s == "/") &&
            isIntegral!Lhs)
    {
        MpZ y = null; // TODO avoid if !__traits(isRef, this)
        version(ccc) ++y._ccc;
        assert(this != 0, "Divison by zero");
        __gmpz_tdiv_q(y._ptr, MpZ(lhs)._ptr, _ptr);
        static      if (isSigned!Lhs)
        {
            return cast(typeof(return))y;
        }
        else static if (isUnsigned!Lhs)
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
    MpZ opBinaryRight(string s, Base)(Base base) const
        if ((s == "^^") &&
            isIntegral!Base)
    {
        static assert(false, "Convert `this MpZ` exponent to `ulong` and calculate power via static method `pow()`");
        // MpZ exp = null;
        // __gmpz_pow();
        // return exp;
    }

    ref MpZ opOpAssign(string s)(auto ref const MpZ rhs) return @trusted // TODO scope
        if ((s == "+" || s == "-" ||
             s == "*" || s == "/" || s == "%" ||
             s == "&" || s == "|" || s == "^"))
    {
        static      if (s == "+")
        {
            __gmpz_add(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
        }
        else static if (s == "-")
        {
            __gmpz_sub(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
        }
        else static if (s == "*")
        {
            if (rhs == -1)
            {
                negate();
            }
            else
            {
                __gmpz_mul(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
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
                __gmpz_tdiv_q(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
            }
        }
        else static if (s == "%")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_r(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
        }
        else static if (s == "&")
        {
            __gmpz_and(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
        }
        else static if (s == "|")
        {
            __gmpz_ior(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
        }
        else static if (s == "^")
        {
            __gmpz_xor(_ptr, _ptr, rhs._ptr); version(ccc) ++_ccc;
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    ref MpZ opOpAssign(string s, Rhs)(Rhs rhs) return @trusted // TODO scope
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%" || s == "^^") &&
            isUnsigned!Rhs)
    {
        static      if (s == "+")
        {
            __gmpz_add_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
        }
        else static if (s == "-")
        {
            __gmpz_sub_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
        }
        else static if (s == "*")
        {
            __gmpz_mul_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
        }
        else static if (s == "/")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_q_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
        }
        else static if (s == "%")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_r_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
        }
        else static if (s == "^^")
        {
            __gmpz_pow_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    ref MpZ opOpAssign(string s, Rhs)(Rhs rhs) return @trusted // TODO scope
        if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%" || s == "^^") &&
            isSigned!Rhs)
    {
        static      if (s == "+")
        {
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                assert(rhs != rhs.min);
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_sub_ui(_ptr, _ptr, pos_rhs); version(ccc) ++_ccc;
            }
            else
            {
                __gmpz_add_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
            }
        }
        else static if (s == "-")
        {
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                assert(rhs != rhs.min);
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_add_ui(_ptr, _ptr, pos_rhs); version(ccc) ++_ccc;
            }
            else
            {
                __gmpz_sub_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
            }
        }
        else static if (s == "*")
        {
            if (rhs == -1)
            {
                negate();       // optimization
            }
            else
            {
                __gmpz_mul_si(_ptr, _ptr, rhs); version(ccc) ++_ccc;
            }
        }
        else static if (s == "/")
        {
            assert(rhs != 0, "Divison by zero");
            if (rhs < 0)        // TODO handle `rhs == rhs.min`
            {
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_tdiv_q_ui(_ptr, _ptr, pos_rhs); version(ccc) ++_ccc;
                negate();
            }
            else
            {
                __gmpz_tdiv_q_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
            }
        }
        else static if (s == "%")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_r_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
        }
        else static if (s == "^^")
        {
            assert(rhs >= 0, "Negative power exponent");
            __gmpz_pow_ui(_ptr, _ptr, rhs); version(ccc) ++_ccc;
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

    /// Returns: negation of `this`.
    MpZ unaryMinus() const @safe
    {
        pragma(msg, "memberFun:", __traits(isRef, this) ? "ref" : "non-ref", " this");
        typeof(return) y = this.dup;
        y.negate();
        return y;
    }

    /** Negate `this` in-place.
        Returns: `void` to make it obvious `this` is mutated.
     */
    void negate() @safe
    {
        _z._mp_size = -_z._mp_size; // fast C macro `mpz_neg` in gmp.h
    }

    /** Make `this` the absolute value of itself in-place.
        Returns: `void` to make it obvious `this` is mutated.
     */
    void absolute() @trusted
    {
        __gmpz_abs(_ptr, _ptr); version(ccc) ++_ccc;
    }

    /// Increase `this` by one.
    ref MpZ opUnary(string s)() return @trusted // TODO scope
        if (s == "++")
    {
        __gmpz_add_ui(_ptr, _ptr, 1); version(ccc) ++_ccc;
        return this;
    }

    /// Decrease `this` by one.
    ref MpZ opUnary(string s)() return @trusted // TODO scope
        if (s == "--")
    {
        __gmpz_sub_ui(_ptr, _ptr, 1); version(ccc) ++_ccc;
        return this;
    }

    /// Returns: `base` raised to the power of `exp`.
    static typeof(this) pow(Base, Exp)(Base base, Exp exp) @trusted
        if (isIntegral!Base &&
            isIntegral!Exp)
    {
        static if (isSigned!Base)
        {
            immutable bool negate = base < 0;
            immutable ubase = cast(ulong)(negate ? -base : base);
        }
        else
        {
            immutable bool negate = false;
            immutable ubase = base;
        }

        static if (isSigned!Exp)
        {
            assert(exp >= 0, "Negative power exponent"); // TODO return mpq?
            immutable uexp = cast(ulong)exp;
        }
        else
        {
            immutable uexp = exp;
        }

        typeof(return) y = null;
        __gmpz_ui_pow_ui(y._ptr, ubase, uexp); version(ccc) ++y._ccc;
        if (negate && exp & 1)  // if negative odd exponent
        {
            y.negate();
        }

        return y;
    }

    /// Returns: number of digits in base `base`.
    size_t sizeInBase(uint base) const @trusted
    {
        return __gmpz_sizeinbase(_ptr, base);
    }

    /// Returns: `true` iff `this` fits in a `T`.
    bool fitsIn(T)() const @trusted
        if (isIntegral!T)
    {
        static      if (is(T == ulong))  { return __gmpz_fits_ulong_p(_ptr) != 0; }
        else static if (is(T ==  long))  { return __gmpz_fits_slong_p(_ptr) != 0; }
        else static if (is(T ==  uint))  { return __gmpz_fits_uint_p(_ptr) != 0; }
        else static if (is(T ==   int))  { return __gmpz_fits_sint_p(_ptr) != 0; }
        else static if (is(T == ushort)) { return __gmpz_fits_ushort_p(_ptr) != 0; }
        else static if (is(T ==  short)) { return __gmpz_fits_sshort_p(_ptr) != 0; }
        else { static assert(false, "Unsupported type " ~ T.stringof); }
    }

    /// Check if `this` is zero.
    @property bool isZero() const @safe
    {
        return _z._mp_size == 0; // fast
    }

    /// Check if `this` is odd. TODO use as specialcase in: this & 1
    @property bool isOdd() const @safe
    {
        return (_z._mp_size != 0) & cast(int)(_z._mp_d[0]); // fast C macro `mpz_odd_p` in gmp.h
    }

    /// Check if `this` is odd.
    @property bool isEven() const @safe
    {
        return !isOdd;            // fast C macro `mpz_even_p` in gmp.h
    }

    /// Check if `this` is negative. TODO use as specialcase in opCmp
    @property bool isNegative() const @safe
    {
        return _z._mp_size < 0; // fast
    }

    /// Check if `this` is positive.
    @property bool isPositive() const @safe
    {
        return !isNegative;     // fast
    }

    /** Returns: sign as either
        - -1 (`this` < 0),
        -  0 (`this` == 0), or
        - +1 (`this` > 0).
     */
    @property int sgn() const @safe
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

    /// Returns: evaluation of `this` expression which in this is a no-op.
    ref inout(MpZ) eval() @safe inout return { return this; }

    /// Type of limb in internal representation.
    alias Limb = __mp_limb_t;   // GNU MP alias

    /** Returns: limbs. */
    inout(Limb)[] _limbs() inout return @system // TODO scope
    {
        // import std.math : abs;
        return _z._mp_d[0 .. _limbCount];
    }

    /// Get number of limbs in internal representation.
    @property uint _limbCount() const @safe
    {
        return _integralAbs(_z._mp_size);
    }

    /// @nogc-variant of `toStringz` with heap allocation of null-terminated C-string `stringz`.
    char* _allocStringzCopyOf(const string value) @trusted @nogc
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
    inout(__mpz_struct)* _ptr() inout return @system // TODO scope
    {
        return &_z;
    }

    __mpz_struct _z;            // internal libgmp C struct

    // qualified C memory managment
    static @safe
    {
        pragma(mangle, "malloc") void* qualifiedMalloc(size_t size);
        pragma(mangle, "free") void qualifiedFree(void* ptr);
    }

    /** Number of calls made to `__gmpz`--functions that construct or changes
        this value. Used to verify correct lowering and evaluation of template
        expressions.

        For instance the `x` in `x = y + z` should be assigned only once inside
        a call to `mpz_add`.
     */
    @property size_t mutatingCallCount() const @safe { return _ccc; }

    version(ccc) size_t _ccc;  // C mutation call count. number of calls to C GMP function calls that mutate this object
}

version(unittest) static assert(isMpZExpr!MpZ);

pure nothrow pragma(inline, true):

/** Instantiator for `MpZ`. */
MpZ mpz(Args...)(Args args) @safe @nogc
{
    return typeof(return)(args);
}

/// Swap contents of `x` with contents of `y`.
void swap()(ref MpZ x,
            ref MpZ y) @nogc
{
    import std.algorithm.mutation : swap;
    swap(x, y); // x.swap(y);
}

/// Returns: `x` as a `string` in decimal base.
string toDecimalString()(auto ref const MpZ x) // for `std.bigint.BigInt` compatibility
{
    return x.toString(10);
}

/// Returns: `x` as a uppercased `string` in hexadecimal base without any base prefix (0x).
string toHex()(auto ref const MpZ x) // for `std.bigint.BigInt` compatibility
{
    return x.toString(16, true);
}

/// Returns: the absolute value of `x` converted to the corresponding unsigned type.
Unsigned!T absUnsign(T)(auto ref const MpZ x) // for `std.bigint.BigInt` compatibility
    if (isIntegral!T)
{
    return _integralAbs(cast(T)x);
}

/** Returns: absolute value of `x`.
    Written as a free function instead of `MpZ`-member because `__traits(isRef, this)` cannot be used.
 */
MpZ abs()(auto ref const MpZ x) @trusted @nogc
{
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
        __gmpz_abs(y._ptr, x._ptr); version(ccc) ++y._ccc;
        return y;
    }
    else                        // r-value `x`
    {
        MpZ* mut_x = (cast(MpZ*)(&x)); // @trusted because `MpZ` has no aliased indirections
        mut_x.absolute();
        return move(*mut_x);    // TODO shouldn't have to call `move` here
    }
}

/// Returns: comparison of the absolute values of `x` and `y`.
int cmpabs()(auto ref const MpZ x,
             auto ref const MpZ y) @trusted @nogc
{
    return __gmpz_cmpabs(x._ptr, y._ptr);
}
/// ditto
int cmpabs()(auto ref const MpZ x, double y) @trusted @nogc
{
    return __gmpz_cmpabs_d(x._ptr, y);
}
/// ditto
int cmpabs()(auto ref const MpZ x, ulong y) @trusted @nogc
{
    return __gmpz_cmpabs_ui(x._ptr, y);
}

/** Returns: next prime greater than `x`.
    Written as a free function instead of `MpZ`-member because `__traits(isRef, this)` cannot be used.
*/
MpZ nextPrime()(auto ref const MpZ x) @trusted @nogc
{
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
        __gmpz_nextprime(y._ptr, x._ptr); version(ccc) ++y._ccc;
        return y;
    }
    else                        // r-value `x`
    {
        MpZ* mut_x = (cast(MpZ*)(&x)); // @trusted because `MpZ` has no aliased indirections
        __gmpz_nextprime(mut_x._ptr, x._ptr); version(ccc) ++mut_x._ccc;
        return move(*mut_x);    // TODO shouldn't have to call `move` here
    }
}

/// Returns: greatest common divisor (gcd) of `x` and `y`.
MpZ gcd()(auto ref const MpZ x,
          auto ref const MpZ y) @trusted @nogc
{
    static      if (!__traits(isRef, x)) // r-value `x`
    {
        MpZ* mut_x = (cast(MpZ*)(&x)); // @trusted because `MpZ` has no aliased indirections
        __gmpz_gcd(mut_x._ptr, x._ptr, y._ptr); version(ccc) ++mut_x._ccc;
        return move(*mut_x);    // TODO shouldn't have to call `move` here
    }
    else static if (!__traits(isRef, y)) // r-value `y`
    {
        MpZ* mut_y = (cast(MpZ*)(&y)); // @trusted because `MpZ` has no aliased indirections
        __gmpz_gcd(mut_y._ptr, x._ptr, y._ptr); version(ccc) ++mut_y._ccc;
        return move(*mut_y);    // TODO shouldn't have to call `move` here
    }
    else                        // l-value `x` and `y`
    {
        MpZ z = null;
        __gmpz_gcd(z._ptr, x._ptr, y._ptr); version(ccc) ++z._ccc;
        return z;
    }
}
/// ditto
MpZ gcd()(auto ref const MpZ x,
          ulong y) @trusted @nogc
{
    static if (__traits(isRef, x)) // l-value `x`
    {
        MpZ z = null;
        const z_ui = __gmpz_gcd_ui(z._ptr, x._ptr, y); version(ccc) ++z._ccc;
        return z;
    }
    else
    {
        MpZ* mut_x = (cast(MpZ*)(&x)); // @trusted because `MpZ` has no aliased indirections
        const z_ui = __gmpz_gcd_ui(mut_x._ptr, x._ptr, y); version(ccc) ++mut_x._ccc;
        return move(*mut_x);    // TODO shouldn't have to call `move` here
    }
}

/// Returns: least common multiple (lcm) of `x` and `y`.
MpZ lcm()(auto ref const MpZ x,
          auto ref const MpZ y) @trusted @nogc
{
    static      if (!__traits(isRef, x)) // r-value `x`
    {
        MpZ* mut_x = (cast(MpZ*)(&x)); // @trusted because `MpZ` has no aliased indirections
        __gmpz_lcm(mut_x._ptr, x._ptr, y._ptr); version(ccc) ++mut_x._ccc;
        return move(*mut_x);    // TODO shouldn't have to call `move` here
    }
    else static if (!__traits(isRef, y)) // r-value `y`
    {
        MpZ* mut_y = (cast(MpZ*)(&y)); // @trusted because `MpZ` has no aliased indirections
        __gmpz_lcm(mut_y._ptr, x._ptr, y._ptr); version(ccc) ++mut_y._ccc;
        return move(*mut_y);    // TODO shouldn't have to call `move` here
    }
    else                        // l-value `x` and `y`
    {
        MpZ z = null;
        __gmpz_lcm(z._ptr, x._ptr, y._ptr); version(ccc) ++z._ccc;
        return z;
    }
}
/// ditto
MpZ lcm()(auto ref const MpZ x,
          ulong y) @trusted @nogc
{
    static if (__traits(isRef, x)) // l-value `x`
    {
        MpZ z = null;
        __gmpz_lcm_ui(z._ptr, x._ptr, y);
        return z;
    }
    else
    {
        MpZ* mut_x = (cast(MpZ*)(&x)); // @trusted because `MpZ` has no aliased indirections
        __gmpz_lcm_ui(mut_x._ptr, x._ptr, y); version(ccc) ++mut_x._ccc;
        return move(*mut_x);    // TODO shouldn't have to call `move` here
    }
}

/** Returns: `base` ^^ `exp` (modulo `mod`).
    Parameter `exp` must be positive.
*/
MpZ powm()(auto ref const MpZ base,
           auto ref const MpZ exp,
           auto ref const MpZ mod) @trusted
{
    typeof(return) y = 0; // result, TODO reuse `exp` or `mod` if any is an r-value
    assert(exp >= 0, "Negative exponent");
    __gmpz_powm(y._ptr, base._ptr, exp._ptr, mod._ptr); version(ccc) ++y._ccc;
    return y;
}
/// ditto
MpZ powm()(auto ref const MpZ base,
           ulong exp,
           auto ref const MpZ mod) @trusted
{
    typeof(return) y = 0;       // result, TODO reuse `exp` or `mod` if any is an r-value
    __gmpz_powm_ui(y._ptr, base._ptr, exp, mod._ptr); version(ccc) ++y._ccc;
    return y;
}

///
@safe @nogc unittest
{
    const x = 42.Z;
    assert(x.unaryMinus() == -42);   // l-value `this`
    assert(42.Z.unaryMinus() == -42); // r-value `this`
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

/// opBinary with r-value right-hand-side
@safe @nogc unittest
{
    Z a = 42;
    {
        Z b = a + 1.Z;              // r-value `rhs`
        assert(b.mutatingCallCount == 2);
        assert(b == 43);
    }

    {
        Z b = a - 1.Z;              // r-value `rhs`
        assert(b.mutatingCallCount == 2);
        assert(b == 41);
    }

    {
        Z b = a * 2.Z;              // r-value `rhs`
        assert(b.mutatingCallCount == 2);
        assert(b == 84);
    }

    {
        Z b = a / 2.Z;              // r-value `rhs`
        assert(b.mutatingCallCount == 2);
        assert(b == 21);
    }

    {
        Z b = a % 10.Z;              // r-value `rhs`
        assert(b.mutatingCallCount == 2);
        assert(b == 2);
    }
}

///
@safe @nogc unittest
{
    const _ = (cast(uint)42).Z;
    const a = 42.Z;
    const b = 43UL.Z;
    const c = 43.0.Z;
    const z = 0.Z;

    // `opOpAssign` with `Unsigned`
    auto w = 42.Z;
    assert(w == 42);

    w += 100UL;
    assert(w == 142);

    w -= 100UL;
    assert(w == 42);

    w *= 100UL;
    assert(w == 4200);

    w /= 100UL;
    assert(w == 42);

    w %= 10UL;
    assert(w == 2);

    w ^^= 6UL;
    assert(w == 64);

    w = 42;
    assert(w == 42);

    w += 100;
    assert(w == 142);

    w -= 100;
    assert(w == 42);

    w *= 100;
    assert(w == 4200);

    w /= 100;
    assert(w == 42);

    w *= 100;
    assert(w == 4200);

    w /= -100;
    assert(w == -42);

    w *= -1;
    assert(w == 42);

    w %= 10;
    assert(w == 2);

    w = 2;
    w ^^= 6;
    assert(w == 64);

    w = 32.0;
    assert(w == 32);

    w = 42UL;
    assert(w == 42);

    // equality
    assert(z == 0);
    assert(z == cast(uint)0);
    assert(z == 0L);
    assert(z == 0UL);
    assert(z == 0.0f);
    assert(z == 0.0);

    // eval cast

    assert(a);
    assert(cast(ulong)a == a);
    assert(cast(ulong)a == 42);
    assert(cast(long)a == a);
    assert(cast(long)a == 42);
    assert(cast(double)a == 42.0);

    // binary

    assert(`0b11`.Z == 3);
    assert(`0B11`.Z == 3);

    // octal

    assert(`07`.Z == 7);
    assert(`010`.Z == 8);

    // hexadecimal

    assert(`0x10`.Z == 16);
    assert(`0X10`.Z == 16);

    // decimal

    assert(`101`.Z == 101);
    assert(`101`.Z == 101);

    immutable ic = 101UL.Z;

    assert(a == a.dup);
    assert(ic == ic.dup);

    // equality

    assert(a == a);
    assert(a == 42.Z);
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
    assert(a < 43.Z);
    assert(a < 43);
    assert(a < cast(uint)43);
    assert(a < 43UL);
    assert(a < 43.0);

    assert(-1.Z < 0.Z);
    assert(-1.Z < 0L);
    assert(-1.Z < 0UL);
    assert(-1.Z < 0.0);

    // greater than

    assert(b > a);
    assert(b > 42.Z);
    assert(b > 42);
    assert(b > cast(uint)42);
    assert(b > 42UL);
    assert(b > 42.0);

    assert(+1.Z > 0.Z);
    assert(+1.Z > 0L);
    assert(+1.Z > 0UL);
    assert(+1.Z > 0.0);

    // absolute value

    assert(abs(a) == a);        // free function
    assert(a.abs == a);         // UFCS
    assert(abs(-42.Z) == 42);
    assert(abs(-a) == a);

    // absolute value comparison

    assert(cmpabs(-43.Z,  44.Z) == -1);
    assert(cmpabs(-43.Z, -44.Z) == -1);
    assert(cmpabs(-44.Z, -43.Z) == +1);
    assert(cmpabs(-43.Z, -43.Z) ==  0);

    assert(cmpabs(-43.Z,  44.0) == -1);
    assert(cmpabs(-43.Z, -44.0) == -1);
    assert(cmpabs(-44.Z, -43.0) == +1);
    assert(cmpabs(-43.Z, -43.0) ==  0);

    assert(cmpabs(-43.Z, 44) == -1);
    assert(cmpabs( 43.Z, 44) == -1);
    assert(cmpabs(-44.Z, 43) == +1);
    assert(cmpabs( 44.Z, 43) == +1);
    assert(cmpabs(-43.Z, 43) ==  0);

    Z _43 = 43;
    Z _4 = 4;
    Z _24 = 24;

    // next prime
    assert(nextPrime(_4) == 5);
    assert(nextPrime(24.Z) == 29);

    assert(nextPrime(_24) == 29);
    assert(nextPrime(24.Z) == 29);
    assert(24.Z.nextPrime() == 29);

    assert(nextPrime(_43) == 47);
    assert(nextPrime(43.Z) == 47);
    assert(43.Z.nextPrime() == 47);

    // greatest common divisor

    assert(gcd(43.Z,  44.Z) == 1);
    assert(gcd(4.Z, 24.Z) == 4);
    assert(gcd(6.Z, 24.Z) == 6);
    assert(gcd(10.Z, 100.Z) == 10);

    assert(gcd(43.Z,  44) == 1);
    assert(gcd(4.Z, 24) == 4);
    assert(gcd(6.Z, 24) == 6);
    assert(gcd(10.Z, 100) == 10);

    assert(gcd(_43,  44) == 1);
    assert(gcd(_4, 24) == 4);
    assert(gcd(_4, _24) == 4);
    assert(gcd(_4, 24.Z) == 4);

    // least common multiple

    assert(lcm(43.Z,  44.Z) == 1892);
    assert(lcm(4.Z, 24.Z) == 24);
    assert(lcm(6.Z, 24.Z) == 24);
    assert(lcm(10.Z, 100.Z) == 100);

    assert(lcm(43.Z,  44) == 1892);
    assert(lcm(4.Z, 24) == 24);
    assert(lcm(6.Z, 24) == 24);
    assert(lcm(10.Z, 100) == 100);

    assert(lcm(_43,  44) == 1892);
    assert(lcm(_4, 24) == 24);
    assert(lcm(_4, _24) == 24);
    assert(lcm(_4, 24.Z) == 24);

    // negated value

    assert(-a == -42);
    assert(-(-a) == a);

    auto n = 42.Z;
    n.negate();
    assert(n == -42);
    n.negate();
    assert(n == 42);
    n.negate();
    assert(n == -42);
    n.absolute();
    assert(n == 42);
    n.absolute();
    assert(n == 42);

    // addition

    assert(a + b == b + a);     // commutative
    assert(a + 43.Z == b + a);
    assert(a - 43.Z == -(43.Z - a));
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
    assert(44UL - 42.Z == 2);

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

    assert(27.Z / 3.Z == 9);
    assert(27.Z /   3  == 9);

    assert(27.Z / 10.Z  == 2);
    assert(27.Z /   10   == 2);
    assert(27.Z /   10UL == 2);

    assert(27.Z / -3   == -9);
    assert(27.Z /  3UL ==  9);

    assert(27.Z / -10   == -2);

    assert(28   /  3.Z ==  9);
    assert(28UL /  3.Z ==  9);

    assert(28   / -3.Z == -9);
    assert(28UL / -3.Z == -9);

    // modulo/remainder

    assert(27.Z % 3.Z == 0);
    assert(27.Z % 10.Z == 7);

    assert(27.Z % 3 == 0);
    assert(-27.Z % 3 == 0);

    assert(27.Z % 10 == 7);
    assert(27.Z % 10 == 7);

    assert(28   % 3.Z == 1);
    assert(28UL % 3.Z == 1);

    assert( 28.Z  % -3 == -1); // negative divisor gives negative remainder according to https://en.wikipedia.org/wiki/Remainder
    assert(-28.Z  %  3 == 1);  // dividend sign doesn't affect remainder

    //
    assert( 28.Z  % -3.Z == 1);  // TODO should be -1
    assert(-28.Z  %  3.Z == -1);  // TODO should be 1
    assert( 28  % -3.Z == 1);      // TODO should be -1
    assert(-28  %  3.Z == -1);     // TODO should be 1

    // modulo/remainder

    immutable one = 1.Z;
    const two = 2.Z;
    immutable three = 3.Z;
    const four = 4.Z;
    immutable five = 5.Z;
    const six = 6.Z;
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

    assert(0.Z^^0 == 1);
    assert(3.Z^^3 == 27);
    assert(3.Z^^3L == 27);
    assert(2.Z^^8 == 256);
    assert(2.Z^^8L == 256);
    assert(2.Z^^8UL == 256);

    assert(Z.pow(2UL, 8UL) == 256);
    assert(Z.pow(2UL, 8) == 256);
    assert(Z.pow(2UL, 8) == 256);
    assert(Z.pow(2, 8) == 256);
    assert(Z.pow(-2, 8) == 256);
    assert(Z.pow(-2, 7) == -128);

    // disallow power exponent to be an `MpZ`
    assert(!__traits(compiles, 2^^8.Z == 256));
    assert(!__traits(compiles, 2L^^8.Z == 256));
    assert(!__traits(compiles, 2UL^^8.Z == 256));

    // exponentiation plus modulus

    assert(2.Z.powm(8.Z, 8.Z) == 0.Z);
    assert(2.Z.powm(3.Z, 16.Z) == 8.Z);
    assert(3.Z.powm(3.Z, 16.Z) == 11.Z);

    assert(2.Z.powm(8, 8.Z) == 0.Z);
    assert(2.Z.powm(3, 16.Z) == 8.Z);
    assert(3.Z.powm(3, 16.Z) == 11.Z);

    // bitwise and, or and xor

    {
        foreach (immutable i; 0 .. 10)
        {
            foreach (immutable j; 0 .. 10)
            {
                assert((i.Z & j.Z) == (i & j));
                assert((i.Z | j.Z) == (i | j));
                assert((i.Z ^ j.Z) == (i ^ j));

                Z x = null;

                x = i.Z;
                x &= j.Z;
                assert(x == (i & j));

                x = i.Z;
                x |= j.Z;
                assert(x == (i | j));

                x = i.Z;
                x ^= j.Z;
                assert(x == (i ^ j));
            }
        }
    }

    // swap

    auto x = 42.Z;
    auto y = 43.Z;

    assert(x == 42);
    assert(y == 43);

    x.swap(y);

    assert(y == 42);
    assert(x == 43);

    swap(x, y);

    assert(x == 42);
    assert(y == 43);

    assert(null.Z.fromString("42") == 42.Z);
    assert(null.Z.fromString("42") < 43.Z);
    assert(null.Z.fromString("42") > 41.Z);
    assert(null.Z.fromString("42") == 42);
    assert(null.Z.fromString("11", 2) == 3);
    assert(null.Z.fromString("7", 8) == 7);
    assert(null.Z.fromString("7") == 7);
    assert(null.Z.fromString("e", 16) == 14);
    assert(null.Z.fromString("f", 16) == 15);
    assert(null.Z.fromString("0xe") == 14);
    assert(null.Z.fromString("0xf") == 15);
    assert(null.Z.fromString("10", 16) == 16);
    assert(null.Z.fromString("10", 32) == 32);

    // odd and even

    assert(0.Z.isEven);
    assert(1.Z.isOdd);
    assert(2.Z.isEven);
    assert(3.Z.isOdd);

    assert((-1).Z.isOdd);
    assert((-2).Z.isEven);
    assert((-3).Z.isOdd);

    assert("300000000000000000000000000000000000000".Z.isEven);
    assert("300000000000000000000000000000000000001".Z.isOdd);
    assert("300000000000000000000000000000000000002".Z.isEven);
    assert("300000000000000000000000000000000000003".Z.isOdd);

    // negative and positive

    assert(0.Z.isPositive);
    assert(1.Z.isPositive);
    assert(2.Z.isPositive);
    assert(3.Z.isPositive);

    assert((-1).Z.isNegative);
    assert((-2).Z.isNegative);
    assert((-3).Z.isNegative);

    // sign function (sgn)

    assert(long.min.Z.sgn == -1);
    assert(int.min.Z.sgn  == -1);
    assert(-2.Z.sgn == -1);
    assert(-1.Z.sgn == -1);
    assert( 0.Z.sgn ==  0);
    assert( 1.Z.sgn ==  1);
    assert( 2.Z.sgn ==  1);
    assert(int.max.Z.sgn  == 1);
    assert(long.max.Z.sgn == 1);

    assert(!long.min.Z.isZero);
    assert(!int.min.Z.isZero);
    assert(!(-2).Z.isZero);
    assert(!(-1).Z.isZero);
    assert( 0.Z.isZero);
    assert(! 1.Z.isZero);
    assert(! 2.Z.isZero);
    assert(!int.max.Z.isZero);
    assert(!long.max.Z.isZero);

    // fits in type

    foreach (Integral; AliasSeq!(short, int, long,
                                 ushort, uint, ulong))
    {
        assert(Integral.min.Z.fitsIn!Integral);
        assert(Integral.max.Z.fitsIn!Integral);
    }

    // TODO
    // assert(short.min.Z.fitsIn!short);
    // assert(short.max.Z.fitsIn!short);
    // assert(ushort.min.Z.fitsIn!ushort);
    // assert(ushort.max.Z.fitsIn!ushort);

    // internal limb count

    assert(0.Z._limbCount == 0);
    assert(1.Z._limbCount == 1);
    assert(2.Z._limbCount == 1);

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
@safe @nogc unittest
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
        immutable int z = 12_345;
        immutable int w = 54_321;
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
        assert(cast(ushort)BigInt(65_535) == 65_535);
        assert(cast(uint)BigInt(uint.max) == uint.max);
        assert(cast(ulong)BigInt(ulong.max) == ulong.max);

        assert(cast(byte)BigInt(-128) == -128);
        assert(cast(short)BigInt(-32_768) == -32_768);
        assert(cast(int)BigInt(int.min) == int.min);
        assert(cast(long)BigInt(long.min) == long.min);

        assert(cast(byte)BigInt(127) == 127);
        assert(cast(short)BigInt(32_767) == 32_767);
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
        const x = BigInt("100");
        const y = BigInt("10");
        const int z = 50;
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
@safe @nogc unittest
{
    if (unittestLong) // compile but not run unless flagged for because running is slow
    {
        /*
          Fermats little theorem: a ^ p ≡ a (mod p) ∀ prime p check Fermats
          little theorem for a ≤ 100000 and all mersene primes M(p) : p ≤ 127
        */
        foreach (immutable ulong i; [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127])
        {
            foreach (immutable ulong j; 2 .. 100_000)
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
pure @nogc unittest
{
    /*
      a^5 + b^5 + c^5 + d^5 = e^5 Lander & Parkin, 1966 found the first counter
      example: 27^5 + 84^5 + 110^5 + 133^5 = 144^5. This test searches for this
      counter example by brute force for all positive a, b, c, d ≤ 144
    */

    enum LIMIT = 144 + 1;
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
                        __gmpz_rootrem(r1._ptr, rem._ptr, r1._ptr, POWER); ++r1._ccc;
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

/// expression template types

/// `MpZ`-`MpZ` adding expression.
struct MpzAddExpr(T1, T2)
    if (isMpZExpr!T1 &&
        isMpZExpr!T2)
{
    T1 e1;                      // first term
    T2 e2;                      // second term
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_add(y._ptr, e1.eval()._ptr, e2.eval()._ptr); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzAddExpr!(MpZ, MpZ)));

/// Instantiator for `MpzAddExpr`.
MpzAddExpr!(T1, T2) mpzAddExpr(T1, T2)(T1 t1, T2 t2)
    if (isMpZExpr!T1 &&
        isMpZExpr!T2)
{
    // TODO don't eval certain type combinations of t1 and t2
    return MpzAddExpr!(T1, T2)(t1.eval(), t2.eval());
}

@safe @nogc unittest
{
    assert(MpzAddExpr!(Z, Z)(3.Z, 4.Z).eval() == 3 + 4);

    const Z x = MpzAddExpr!(Z, Z)(3.Z, 4.Z);
    assert(x.mutatingCallCount == 1); // lower to `mpz_add`
    assert(x == 7);

    Z y = null;
    y = MpzAddExpr!(Z, Z)(3.Z, 4.Z);
    assert(y.mutatingCallCount == 2); // lowers to `mpz_init`, `mpz_add`
    assert(y == 7);

}

/// `MpZ`-`MpZ` subtraction expression.
struct MpzSubExpr(T1, T2)
    if (isMpZExpr!T1 &&
        isMpZExpr!T2)
{
    T1 e1;                      // first term
    T2 e2;                      // second term
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_sub(y._ptr, e1.eval()._ptr, e2.eval()._ptr); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzSubExpr!(MpZ, MpZ)));

@safe @nogc unittest
{
    assert(MpzSubExpr!(Z, Z)(3.Z, 4.Z).eval() == 3 - 4);
    const Z x = MpzSubExpr!(Z, Z)(3.Z, 4.Z);
    assert(x.mutatingCallCount == 1); // lowers to `mpz_sub`
    assert(x == -1);
}

/// `MpZ`-`MpZ` multiplication expression.
struct MpzMulExpr(F1, F2)
    if (isMpZExpr!F1 &&
        isMpZExpr!F2)
{
    F1 e1;                      // first factor
    F2 e2;                      // second factor
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_mul(y._ptr, e1.eval()._ptr, e2.eval()._ptr); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzMulExpr!(MpZ, MpZ)));

@safe @nogc unittest
{
    assert(MpzMulExpr!(Z, Z)(3.Z, 4.Z).eval() == 3 * 4);

    const Z x = MpzMulExpr!(Z, Z)(3.Z, 4.Z);
    assert(x == 12);
    assert(x.mutatingCallCount == 1); // lowers to `mpz_mul`
}

/// `MpZ`-`MpZ` division expression.
struct MpzDivExpr(P, Q)
    if (isMpZExpr!P &&
        isMpZExpr!Q)
{
    P e1;                       // divisor
    Q e2;                       // dividend
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_tdiv_q(y._ptr, e1.eval()._ptr, e2.eval()._ptr); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzDivExpr!(MpZ, MpZ)));

@safe @nogc unittest
{
    assert(MpzDivExpr!(Z, Z)(27.Z, 3.Z).eval() == 27 / 3);
    assert(MpzDivExpr!(Z, Z)(28.Z, 3.Z).eval() == 28 / 3);
    assert(MpzDivExpr!(Z, Z)(29.Z, 3.Z).eval() == 29 / 3);
    assert(MpzDivExpr!(Z, Z)(30.Z, 3.Z).eval() == 30 / 3);

    const Z x = MpzDivExpr!(Z, Z)(28.Z, 3.Z);
    assert(x == 9);
    assert(x.mutatingCallCount == 1); // lowers to `mpz_tdiv_q`
}

/// `MpZ`-`MpZ` modulus expression.
struct MpzModExpr(P, Q)
    if (isMpZExpr!P &&
        isMpZExpr!Q)
{
    P e1;                       // divisor
    Q e2;                       // dividend
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_tdiv_r(y._ptr, e1.eval()._ptr, e2.eval()._ptr); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzModExpr!(MpZ, MpZ)));

@safe @nogc unittest
{
    assert(MpzModExpr!(Z, Z)(27.Z, 3.Z).eval() == 27 % 3);
    assert(MpzModExpr!(Z, Z)(28.Z, 3.Z).eval() == 28 % 3);
    assert(MpzModExpr!(Z, Z)(29.Z, 3.Z).eval() == 29 % 3);
    assert(MpzModExpr!(Z, Z)(30.Z, 3.Z).eval() == 30 % 3);

    const Z x = MpzModExpr!(Z, Z)(29.Z, 3.Z);
    assert(x == 2);
    assert(x.mutatingCallCount == 1); // lowers to `mpz_tdiv_r`
}

/// `MpZ`-`ulong` power expression.
struct MpzPowUExpr(P, Q)
    if (isMpZExpr!P &&
        isUnsigned!Q)
{
    P e1;                       // base
    Q e2;                       // exponent
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_pow_ui(y._ptr, e1.eval()._ptr, e2); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzPowUExpr!(MpZ, ulong)));

@safe @nogc unittest
{
    assert(MpzPowUExpr!(Z, ulong)(3.Z, 3).eval() == 3^^3);
}

/// `MpZ`-`ulong`-`MpZ` power-modulo expression.
struct MpzPowMUExpr(P, Q, M)
    if (isMpZExpr!P &&
        isUnsigned!Q &&
        isMpZExpr!M)
{
    P base;                     // base
    Q exp;                      // exponent
    M mod;                      // modulo
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_powm_ui(y._ptr, base.eval()._ptr, exp, mod._ptr); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzPowMUExpr!(MpZ, ulong, MpZ)));

@safe @nogc unittest
{
    assert(MpzPowMUExpr!(Z, ulong, Z)(3.Z, 3, 20.Z).eval() == 3^^3 % 20);
}

/// `MpZ` negation expression.
struct MpzNegExpr(A)
    if (isMpZExpr!A)
{
    A e1;
    pragma(inline, true)
    MpZ eval() const @trusted
    {
        typeof(return) y = null;
        __gmpz_neg(y._ptr, e1.eval()._ptr); ++y._ccc;
        return y;
    }
}
version(unittest) static assert(isMpZExpr!(MpzNegExpr!(MpZ)));

@safe @nogc unittest
{
    assert(MpzNegExpr!(Z)(27.Z).eval() == -27);

    const Z x = MpzNegExpr!(Z)(27.Z);
    assert(x.mutatingCallCount == 1); // lowers to `mpz_neg`
    assert(x == -27);
}

/// Copied from `std.numeric` to prevent unnecessary Phobos deps.
T _integralAbs(T)(T x)
    if (isIntegral!T)
{
    return x >= 0 ? x : -x;
}

/** Faster than `std.traits`.
    See https://github.com/dlang/phobos/pull/5038
*/
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
enum isOut(alias fn) = __traits(isOut, fn);
enum isLazy(alias fn) = __traits(isLazy, fn);
enum isTemplate(alias sym) = __traits(isTemplate, sym);
enum hasMember(T, string member) = __traits(hasMember, T, member);
enum IdentifierStringOfSymbol(alias sym) = __traits(identifier, sym);

// TODO WARNING disabled because this cannot be wrapped in a template
enum isRef(alias fn) = __traits(isRef, fn);

/// http://forum.dlang.org/post/llwrbirvlqxawifyytqq@forum.dlang.org
@safe pure nothrow @nogc unittest
{
    struct S { int x, y; }

    static void f()(auto ref const S s)
    {
        static assert(__traits(isRef, s));
    }

    static void g()(auto ref const S s)
    {
        static assert(!__traits(isRef, s));
    }

    S s;
    static assert(!__traits(isRef, s));

    f(s);

    g(S.init);
}

/// as hash table key
@safe unittest
{
    // TODO disabled until non-copyable types work in AA's
    // string[Z] aa;
    // aa[123.Z] = "abc";
    // aa[456.Z] = "def";
    // assert(aa[123.Z] == "abc");
    // assert(aa[456.Z] == "def");
}

version(unittest)
{
    import dbgio;
    alias Z = MpZ;
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

    void __gmpz_swap (mpz_ptr, mpz_ptr);

    int __gmpz_cmp (mpz_srcptr, mpz_srcptr);
    int __gmpz_cmp_d (mpz_srcptr, double);
    int __gmpz_cmp_si (mpz_srcptr, long);
    int __gmpz_cmp_ui (mpz_srcptr, ulong);

    int __gmpz_cmpabs (mpz_srcptr, mpz_srcptr);
    int __gmpz_cmpabs_d (mpz_srcptr, double);
    int __gmpz_cmpabs_ui (mpz_srcptr, ulong);

    void __gmpz_nextprime (mpz_ptr, mpz_srcptr);

    void __gmpz_gcd (mpz_ptr, mpz_srcptr, mpz_srcptr);
    ulong __gmpz_gcd_ui (mpz_ptr, mpz_srcptr, ulong);

    void __gmpz_lcm (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_lcm_ui (mpz_ptr, mpz_srcptr, ulong);

    char *__gmpz_get_str (char*, int, mpz_srcptr);
    size_t __gmpz_sizeinbase (mpz_srcptr, int);

    void __gmpz_powm (mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_powm_ui (mpz_ptr, mpz_srcptr, ulong, mpz_srcptr);

    ulong __gmpz_get_ui (mpz_srcptr);
    long __gmpz_get_si (mpz_srcptr);
    double __gmpz_get_d (mpz_srcptr);

    int __gmpz_fits_ulong_p(mpz_srcptr);
    int __gmpz_fits_slong_p(mpz_srcptr);
    int __gmpz_fits_uint_p(mpz_srcptr);
    int __gmpz_fits_sint_p(mpz_srcptr);
    int __gmpz_fits_ushort_p(mpz_srcptr);
    int __gmpz_fits_sshort_p(mpz_srcptr);

    void __gmpz_and (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_ior (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_xor (mpz_ptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_com (mpz_ptr, mpz_srcptr);

    // TODO wrap:
    void __gmpz_root (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_rootrem (mpz_ptr, mpz_ptr, mpz_srcptr, ulong);

    void __gmpz_sqrt (mpz_ptr, mpz_srcptr);
    void __gmpz_sqrtrem (mpz_ptr, mpz_ptr, mpz_srcptr);

    void __gmpz_perfect_power_p (mpz_ptr, mpz_srcptr);
}

// link with C library GNU MP
pragma(lib, "gmp");
