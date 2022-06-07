/// Multiple precision integers (Z).
module gmp.z;

debug import std.stdio : writeln;
import std.exception : enforce;
import core.lifetime : move;
import std.traits : Unsigned, Unqual, isIntegral, isUnsigned; // used by expression templates
import gmp.traits;

/// Call unittests taking long to execute.
// version = unittestLong;

/// Is `true` if type `T` can be evaluated to a `MpZ` value.
enum isMpZExpr(T) = (is(Unqual!(typeof(T.eval())) == MpZ) ||
					 is(Unqual!(typeof(T.eval())) == CopyableMpZ)); // which returns an `MpZ`
/// Is `true` if type `T` is lazy (not yet evaluated) `MpZ`-expression.
enum isLazyMpZExpr(T) = (!is(Unqual!T == MpZ) &&            // exclude direct value
                         isMpZExpr!T);

/** WordEndianess of serialization in `MpZ.serialize` and
	unserialization-construction from integer array.
 */
enum WordEndianess
{
    host,
    bigEndian,
    littleEndian,
}

/** Word-order of serialization in `MpZ.serialize` and
	unserialization-construction from integer array.
 */
enum WordOrder
{
    mostSignificantWordFirst,
    leastSignificantWordFirst,
}

// TODO: use these imports instead of the ones below
// import deimos.gmp.gmp;
// import deimos.gmp.integer;

/** Arbitrary (multi) precision signed integer (Z).

	Wrapper for GNU MP (GMP)'s type `mpz_t` and functions `__gmpz_.*`.

	If `cow` is `false` copying (via assignment and parameter passing by value)
	is only possible explicitly via `.dup` (Rust-style) otherwise copying is
	automatic and does copy-on-write (CoW) of the internal data via reference
	counting (ARC) (Swift-style).
 */
private struct _Z(bool cow)
{
pure:
    /** Needed below because `mpz_init_set_str` is currently the only way to
	 * construct from an array of characters.
	 */
    private enum smallBufSize = 1024;

    /** Construct from `value` in base `base`.

		If `base` is 0 it's guessed from contents of `value`.
	*/
    this(scope const(char)[] value, uint base = 0) @trusted // TODO: Use Optional/Nullable when value is nan, or inf
        in(base == 0 ||
		   (+2 <= base && base <= +62))
    {
		import std.algorithm.searching : canFind;
		if (value.length >= 2 &&
			value[0] == '0' &&
			((base == 16 &&
			  (value[1] == 'x' ||
			   value[1] == 'X')) ||
			 (base == 2 &&
			  (value[1] == 'b' ||
			   value[1] == 'B')) ||
			 (base == 8 &&
			  (value[1] == 'o' ||
			   value[1] == 'O'))))
		{
			value = value[2 .. $]; // __gmpz_init_set_str doesn’t allow `"0x"` prefix if `base` given
		}
		char[smallBufSize] buf;
		char* stringz;
		if (value.length + 1 <= smallBufSize &&
			value[0] != '-' &&	 // TODO: enable support for this
			!value.canFind('_')) // TODO: enable support for this
		{
			buf[0 .. value.length] = value;
			buf[value.length] = '\0'; // null terminator
			stringz = buf.ptr;
		}
		else
		{
			stringz = _allocStringzCopyOf(value); // TODO: append inline trailing zero if possible otherwise make this stack allocated
		}
		scope(exit)
		{
			if (stringz != buf.ptr)
				qualifiedFree(stringz);
		}
        immutable int status = __gmpz_init_set_str(_ptr, stringz, base); version(ccc) { ++_ccc; }
        enforce(status == 0, "Parameter `value` does not contain an integer");
    }

	static typeof(this) fromBinaryString(scope const(char)[] value) pure @safe
	{
		return typeof(return)(value, 2);
	}

	static typeof(this) fromHexString(scope const(char)[] value) pure @safe
	{
		return typeof(return)(value, 16);
	}

	static typeof(this) fromHexString(string value)() pure @trusted // `value` is guaranteed to be null-terminated
	{
		static if (value.length >= 2 &&
				   (value[0] == '0' &&
					(value[1] == 'x' ||
					 value[1] == 'X')))
			enum adjustedValue = value[2 .. $]; // __gmpz_init_set_str doesn’t allow `"0x"` prefix if `base` given
		else
			enum adjustedValue = value;
		typeof(return) result = void;
        immutable int status = __gmpz_init_set_str(result._ptr, adjustedValue.ptr, 16); version(ccc) { ++_ccc; }
        enforce(status == 0, "Parameter `value` does not contain an integer");
		return result;
	}

nothrow:

    /// Convert to `string` in base `base`.
    string toString(in uint base = defaultBase,
                    in bool upperCaseDigits = false) const @trusted
		in(-2 <= base && base <= -36 ||
		   +2 <= base && base <= +62)
    {
        if (isZero) { return `0`; }
        const size = sizeInBase(base); // NOTE: one too much for some values
        char[] str = new char[](size + 1); // one extra for minus sign
        __gmpz_get_str(str.ptr, base, _ptr); // fill it
		return fillChars(str, base, upperCaseDigits);
    }

    /** Convert in base `base` into `chars` of length `length`.

		Returns: char[] which must be freed manually with `pureFree`.
	*/
    char[] toChars(in uint base = defaultBase,
                   in bool upperCaseDigits = false) const @system @nogc
		in(-2 <= base && base <= -36 ||
		   +2 <= base && base <= +62)
    {
        import core.memory : pureMalloc;
        if (isZero)
        {
            char[] chars = (cast(char*)pureMalloc(1))[0 .. 1];
            chars[0] = '0';
            return chars;
        }
        const size = sizeInBase(base); // NOTE: one too much for some values
        char[] chars = (cast(char*)pureMalloc(size + 1))[0 .. size + 1]; // one extra for minus sign
		return fillChars(chars, base, upperCaseDigits);
    }

    private char[] fillChars(char[] chars,
							 in uint base = defaultBase,
							 in bool upperCaseDigits = false) const @system @nogc
		in(-2 <= base && base <= -36 ||
		   +2 <= base && base <= +62)
    {
        __gmpz_get_str(chars.ptr, base, _ptr); // fill it
        import std.ascii : isAlphaNum, isLower, toUpper;
        while (chars.length &&
               !chars[$ - 1].isAlphaNum)
            chars = chars[0 .. $ - 1]; // skip trailing garbage
        if (upperCaseDigits)
            foreach (ref e; chars)
                if (e.isLower)
                    e = e.toUpper;
		return chars;
	}

	void toString(Writer)(ref Writer writer, // `mir.appender` compliant
						  in uint base = defaultBase,
						  in bool upperCaseDigits = false) const @nogc @trusted
        if (is(typeof(writer.put((const(char)[]).init))))
	{
        import core.memory : pureFree;
        if (isZero) { return writer.put("0"); }
		auto chars = toChars(base, upperCaseDigits);
		scope(exit) pureFree(chars.ptr);
		writer.put(chars);
	}

    /// Get the unique hash of the `_Z` value suitable for use in a hash table.
    size_t toHash() const @trusted
    {
        import core.internal.hash : hashOf;
        typeof(return) hash = limbCount;
        foreach (immutable i; 0 .. limbCount)
            hash ^= _limbs[i].hashOf;
        return hash;
    }

    /** Serialize `this` into a new GC-allocated slice of words, each word of
		type `Word`.

		It's format defined by:
		- `order`: the most significant word `first` or `last` for least significant first
		- `endian` can be `bigEndian`, `littleEndian` or `host` default
		- the most significant `nails` bits of each word are unused and set to zero, this can be 0 to produce full words

		Returns: a new GC-allocated slice containing the words produced.
	*/
    Word[] serialize(Word)(WordOrder order, WordEndianess endian, size_t nails) const @trusted
    if (isUnsigned!Word)
    {
        auto numb = 8 * Word.sizeof - nails;
        size_t count = (__gmpz_sizeinbase(_ptr, 2) + numb-1) / numb;
        return serialize(new Word[count], order, Word.sizeof, endian, nails);
    }

@nogc:

    /** No default construction for now, because `mpz_init` initialize
        `__mpz_struct`-fields to non-zero values.

        TODO: Allow default construction by delaying call to initialize().
    */
    // @disable this();

    /// Construct empty (undefined) from explicit `null`.
    this(typeof(null)) @trusted
    {
        pragma(inline, true);
        initialize();             // TODO: is there a faster way?
        assert(this == _Z.init); // if this is same as default
    }

    /// Construct from expression `expr`.
    this(Expr)(Expr expr)
    if (isLazyMpZExpr!Expr)
    {
        version(LDC) pragma(inline, true);
        // TODO: ok to just assume zero-initialized contents at `_z` before...
        this = expr;            // ...calling this.opAssign ?x
    }

    /** Construct from `value`. */
    this(T)(T value) @trusted
    if (isArithmetic!T)
    {
        // TODO: add support for static initialization
        // if (!__ctfe)
        // {
        pragma(inline, true);
        version(ccc) { ++_ccc; }
        static      if (isUnsigned!T)
            __gmpz_init_set_ui(_ptr, value);
        else static if (isFloating!T)
            __gmpz_init_set_d(_ptr, value);
        else                    // isSigned integral
            __gmpz_init_set_si(_ptr, value);
        // }
        // else
        // {
        //     if (value == 0)
        //     {
        //         reinterpret_to_size_if_sizeof_and_zero();
        //     }
        //     else
        //     {
        //         assert(0, "Cannot set non-zero value at ctfe");
        //     }
        // }
    }

    /** Constucts number from serialized binary array.

		Arguments:
		- `rop` : array of unsinged values
		- `order`: the most significant word `first` or `last` for least significant first
		- `size` in bytes of each word
		- `endian` can be `bigEndian`, `littleEndian` or `host` default
		- the most significant `nails` bits of each word are unused and set to zero, this can be 0 to produce full words
	*/
    this(T)(const T[] rop, WordOrder order, size_t size, WordEndianess endian, size_t nails)
    if (isUnsigned!T)
    {

        int realOrder;
        final switch(order)
        {
            case WordOrder.mostSignificantWordFirst:  realOrder = 1;  break;
            case WordOrder.leastSignificantWordFirst: realOrder = -1; break;
        }

        int realEndian;
        final switch(endian)
        {
            case WordEndianess.littleEndian: realEndian = -1; break;
            case WordEndianess.bigEndian:    realEndian =  1; break;
            case WordEndianess.host:         realEndian =  0; break;
        }

        __gmpz_init(_ptr);
        __gmpz_import(_ptr, rop.length, realOrder, size, realEndian, nails, rop.ptr);
    }

    /// Mersenne prime, M(p) = 2 ^^ p - 1
    static _Z mersennePrime(Integral)(Integral p)
    if (isIntegral!Integral)
    {
        version(LDC) pragma(inline, true);
        return typeof(this).pow(2UL, p) - 1;
    }

	// /// Construct copy of `value`.
	// this(ref const _Z value) @trusted
	// {
	//     __gmpz_init_set(_ptr, value._ptr); version(ccc) { ++_ccc; }
	// }

	// /// Construct copy of `value`.
	// this(_Z value) @trusted
	// {
	// import core.lifetime : moveEmplace;
	//     moveEmplace(value, this); // fast
	// }

	static if (cow)
    {
		void selfdupIfAliased() scope pure nothrow @nogc @safe {
            pragma(inline, true);
			if (_refCountCopies >= 1)
				this = this.dup();
		}
        this(this) scope pure nothrow @nogc @safe
        {
            pragma(inline, true);
			_refCountCopies += 1;
        }
	}
    else
    {
        @disable this(this);
    }

    /// Swap content of `this` with `rhs`.
    void swap()(ref _Z rhs) scope pure nothrow @nogc @safe
    {
        pragma(inline, true);
        import std.algorithm.mutation : swap;
        swap(this, rhs); // faster than __gmpz_swap(_ptr, rhs._ptr); version(ccc) { ++_ccc; }
    }

    /// (Duplicate) Copy `this`.
    _Z dup() const scope pure nothrow @trusted
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = void;
        __gmpz_init_set(y._ptr, _ptr); version(ccc) ++y._ccc;
		static if (cow) { y._refCountCopies = 0; }
        return y;
    }

    /// Assign from `rhs`.
    ref _Z opAssign()(auto ref scope const _Z rhs) scope return @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_set(_ptr, rhs._ptr); version(ccc) { ++_ccc; }
        return this;
    }
    /// ditto
    ref _Z opAssign(Expr)(auto ref Expr rhs) scope return @trusted
    if (isLazyMpZExpr!Expr)
    {
        version(LDC) pragma(inline, true);
        rhs.evalTo(this);
        return this;
    }
    /// ditto
    ref _Z opAssign(T)(T rhs) scope return @trusted
    if (isArithmetic!T)
    {
        version(LDC) pragma(inline, true);
        assertInitialized();
		static if (cow) { selfdupIfAliased(); }
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
            __gmpz_set_si(_ptr, rhs);
        }
        else
        {
            static assert(false);
        }
        version(ccc) { ++_ccc; }
        return this;
    }

    /** Assign `this` from `string` `rhs` interpreted in base `base`.
        If `base` is 0 it's guessed from contents of `value`.
    */
    ref _Z fromString(scope const(char)[] rhs, uint base = 0) scope return @trusted
    {
        assert(base == 0 || (base >= 2 && base <= 62));
		static if (cow) { selfdupIfAliased(); }
        char* stringz = _allocStringzCopyOf(rhs);
        immutable int status = __gmpz_set_str(_ptr, stringz, base); version(ccc) { ++_ccc; }
        qualifiedFree(stringz);
        assert(status == 0, "Parameter `rhs` does not contain an integer");
        return this;
    }

    /// Destruct `this`.
    ~this() @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        if (_z._mp_d)
        {
			static if (cow)
			{
				if (_refCountCopies >= 1)
				{
					_z._mp_d = null;    // prevent GC from scanning this memory
					return;
				}
			}
            __gmpz_clear(_ptr); version(ccc) { ++_ccc; }
            _z._mp_d = null;    // prevent GC from scanning this memory
        }
    }

    /// Returns: `true` iff `this` equals `rhs`.
    bool opEquals()(auto ref scope const _Z rhs) const @trusted
    {
        version(LDC) pragma(inline, true);
        if (_ptr == rhs._ptr)   // fast equality
        {
            return true;        // fast bailout
        }
        return __gmpz_cmp(_ptr, rhs._ptr) == 0;
    }
    /// ditto
    bool opEquals(Rhs)(Rhs rhs) const @trusted
    if (isArithmetic!Rhs)
    {
        pragma(inline, true)
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
            return __gmpz_cmp_d(_ptr, cast(double)rhs) == 0; // TODO: correct to do this cast here?
        }
        else                    // isSigned integral
        {
            return __gmpz_cmp_si(_ptr, cast(long)rhs) == 0;
        }
    }

    /// Compare `this` to `rhs`.
    int opCmp()(auto ref scope const _Z rhs) const @trusted
    {
        version(LDC) pragma(inline, true);
        if (rhs == 0)
        {
            return sgn();       // optimization
        }
        return __gmpz_cmp(_ptr, rhs._ptr);
    }
    /// ditto
    int opCmp(T)(T rhs) const @trusted
    if (isArithmetic!T)
    {
        pragma(inline, true);
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
    bool opCast(T : bool)() const
    {
        pragma(inline, true);
        return !isZero;
    }

    /// Cast to arithmetic type `T`.
    T opCast(T)() const @trusted
    if (isArithmetic!T)
    {
        pragma(inline, true);
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

    /** Get the value of this as a `long`, or +/- `long.max` if outside the
		representable range. */
    long toLong() const @trusted
    {
        version(LDC) pragma(inline, true);
        // TODO: can probably be optimized
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

    /** Get the value of this as a `int`, or +/- `int.max` if outside the
		representable range.
	*/
    int toInt() const @trusted
    {
        version(LDC) pragma(inline, true);
        // TODO: can probably be optimized
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

    /** Get `this` `s` `rhs`. */
    _Z opBinary(string s)(auto ref scope const _Z rhs) const @trusted // direct value
    if ((s == "+" || s == "-" ||
         s == "*" || s == "/" || s == "%" ||
         s == "&" || s == "|" || s == "^" ||
         s == "<<"))            // left shift
    {
        version(LDC) pragma(inline, true);
        static if (!__traits(isRef, rhs)) // r-value `rhs`
        {
            _Z* mut_rhs = (cast(_Z*)(&rhs)); // @trusted because `_Z` has no aliased indirections
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
            else static if (s == "<<")
            {
                const rhs_ulong = cast(ulong)rhs;
                __gmpz_mul_2exp(mut_rhs._ptr, _ptr, rhs_ulong); version(ccc) ++mut_rhs._ccc;
            }
            else
            {
                static assert(false);
            }
            return move(*mut_rhs); // TODO: shouldn't have to call `move` here
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
            else static if (s == "<<")
            {
                const rhs_ulong = cast(ulong)rhs;
                __gmpz_mul_2exp(y._ptr, _ptr, rhs_ulong); version(ccc) ++y._ccc;
            }
            else
            {
                static assert(false);
            }
            return y;
        }
    }

    /// ditto
    _Z opBinary(string s, Rhs)(auto ref const Rhs rhs) const
    if (isLazyMpZExpr!Rhs && // lazy value
        (s == "+" || s == "-" || s == "*" || s == "/" || s == "%"))
    {
        pragma(inline, true);
        static assert(false, "TODO");
    }

    /// ditto
    _Z opBinary(string s, Rhs)(Rhs rhs) const @trusted
    if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "^^" || s == "<<") &&
        isUnsigned!Rhs)
    {
        version(LDC) pragma(inline, true);
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
        else static if (s == "<<")
        {
            __gmpz_mul_2exp(y._ptr, _ptr, rhs); version(ccc) ++rhs._ccc;
        }
        else
        {
            static assert(false);
        }
        return y;
    }

    /// ditto
    _Z opBinary(string s, Rhs)(Rhs rhs) const @trusted
    if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "^^") &&
        isSigned!Rhs)
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        version(ccc) ++y._ccc;
        static      if (s == "+")
        {
            if (rhs < 0)        // TODO: handle `rhs == rhs.min`
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
            if (rhs < 0)        // TODO: handle `rhs == rhs.min`
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
            if (rhs < 0)        // TODO: handle `rhs == rhs.min`
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
            assert(rhs >= 0, "TODO: Negative power exponent needs MpQ return");
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
        version(LDC) pragma(inline, true);
        assert(rhs != 0, "Divison by zero");
        _Z y = null;
        version(ccc) ++y._ccc;
        static if (isSigned!Rhs)
        {
            if (rhs < 0)        // TODO: handle `rhs == rhs.min`
            {
                immutable ulong pos_rhs = -cast(int)rhs; // make it positive
                return cast(typeof(return))-__gmpz_tdiv_r_ui(y._ptr, _ptr, pos_rhs);
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

    /// Get an unsigned type `lhs` divided by `this`.
    _Z opBinaryRight(string s, Lhs)(Lhs lhs) const @trusted
    if ((s == "+" || s == "-" || s == "*" || s == "%") &&
        isUnsigned!Lhs)
    {
        version(LDC) pragma(inline, true);
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
            __gmpz_tdiv_r(y._ptr, _Z(lhs)._ptr, _ptr); // convert `lhs` to _Z
        }
        else
        {
            static assert(false);
        }
        return y;
    }

    /// Get a signed type `lhs` divided by `this`.
    _Z opBinaryRight(string s, Lhs)(Lhs lhs) const @trusted
    if ((s == "+" || s == "-" || s == "*" || s == "%") &&
        isSigned!Lhs)
    {
        version(LDC) pragma(inline, true);
        static if (s == "+" || s == "*")
        {
            return opBinary!s(lhs); // commutative reuse
        }
        else static if (s == "-")
        {
            typeof(return) y = null;
            version(ccc) ++y._ccc;
            if (lhs < 0)        // TODO: handle `lhs == lhs.min`
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
            __gmpz_tdiv_r(y._ptr, _Z(lhs)._ptr, _ptr); // convert `lhs` to _Z
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
        version(LDC) pragma(inline, true);
        _Z y = null; // TODO: avoid if !__traits(isRef, this)
        version(ccc) ++y._ccc;
        assert(this != 0, "Divison by zero");
        __gmpz_tdiv_q(y._ptr, _Z(lhs)._ptr, _ptr);
        static      if (isSigned!Lhs)
        {
            return cast(typeof(return))y;
        }
        else static if (isUnsigned!Lhs)
        {
            import std.traits : Signed;
            return cast(Signed!(typeof(return)))y;
        }
        else
        {
            static assert(false);
        }
    }

    /// Exponentation.
    _Z opBinaryRight(string s, Base)(Base base) const
    if ((s == "^^") &&
        isIntegral!Base)
    {
        pragma(inline, true);
        static assert(false, "Convert `this _Z` exponent to `ulong` and calculate power via static method `pow()`");
        // _Z exp = null;
        // __gmpz_pow();
        // return exp;
    }

    /// Operate-assign to `this` from `rhs`.
    ref _Z opOpAssign(string s)(auto ref scope const _Z rhs) scope return @trusted
    if ((s == "+" || s == "-" ||
         s == "*" || s == "/" || s == "%" ||
         s == "&" || s == "|" || s == "^"))
    {
        version(LDC) pragma(inline, true);
        static      if (s == "+")
        {
            __gmpz_add(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
        }
        else static if (s == "-")
        {
            __gmpz_sub(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
        }
        else static if (s == "*")
        {
            if (rhs == -1)
            {
                negate();
            }
            else
            {
                __gmpz_mul(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
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
                __gmpz_tdiv_q(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
            }
        }
        else static if (s == "%")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_r(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
        }
        else static if (s == "&")
        {
            __gmpz_and(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
        }
        else static if (s == "|")
        {
            __gmpz_ior(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
        }
        else static if (s == "^")
        {
            __gmpz_xor(_ptr, _ptr, rhs._ptr); version(ccc) { ++_ccc; }
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    /// ditto
    ref _Z opOpAssign(string s, Rhs)(Rhs rhs) scope return @trusted
    if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%" || s == "^^") &&
        isUnsigned!Rhs)
    {
        version(LDC) pragma(inline, true);
        static      if (s == "+")
        {
            __gmpz_add_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else static if (s == "-")
        {
            __gmpz_sub_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else static if (s == "*")
        {
            __gmpz_mul_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else static if (s == "/")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_q_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else static if (s == "%")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_r_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else static if (s == "^^")
        {
            __gmpz_pow_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    /// ditto
    ref _Z opOpAssign(string s, Rhs)(Rhs rhs) scope return @trusted
    if ((s == "+" || s == "-" || s == "*" || s == "/" || s == "%" || s == "^^") &&
        isSigned!Rhs)
    {
        version(LDC) pragma(inline, true);
        static      if (s == "+")
        {
            if (rhs < 0)        // TODO: handle `rhs == rhs.min`
            {
                assert(rhs != rhs.min);
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_sub_ui(_ptr, _ptr, pos_rhs); version(ccc) { ++_ccc; }
            }
            else
            {
                __gmpz_add_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
            }
        }
        else static if (s == "-")
        {
            if (rhs < 0)        // TODO: handle `rhs == rhs.min`
            {
                assert(rhs != rhs.min);

                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_add_ui(_ptr, _ptr, pos_rhs); version(ccc) { ++_ccc; }
            }
            else
            {
                __gmpz_sub_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
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
                __gmpz_mul_si(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
            }
        }
        else static if (s == "/")
        {
            assert(rhs != 0, "Divison by zero");
            if (rhs < 0)        // TODO: handle `rhs == rhs.min`
            {
                immutable ulong pos_rhs = -rhs; // make it positive
                __gmpz_tdiv_q_ui(_ptr, _ptr, pos_rhs); version(ccc) { ++_ccc; }
                negate();
            }
            else
            {
                __gmpz_tdiv_q_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
            }
        }
        else static if (s == "%")
        {
            assert(rhs != 0, "Divison by zero");
            __gmpz_tdiv_r_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else static if (s == "^^")
        {
            assert(rhs >= 0, "Negative exponent");
            __gmpz_pow_ui(_ptr, _ptr, rhs); version(ccc) { ++_ccc; }
        }
        else
        {
            static assert(false);
        }
        return this;
    }

    /// Returns: `this`.
    ref inout(_Z) opUnary(string s)() inout scope return
    if (s == "+")
    {
        pragma(inline, true);
        return this;
    }

    /// Negation of `this`.
    _Z opUnary(string s)() const
    if (s == "-")
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = this.dup;
        y.negate();
        return y;
    }

    /// Negation of `this`.
    _Z unaryMinus() const @safe
    {
        version(LDC) pragma(inline, true);
        // pragma(msg, "memberFun:", __traits(isRef, this) ? "ref" : "non-ref", " this");
        typeof(return) y = this.dup;
        y.negate();
        return y;
    }

    /** Negate `this` in-place.

		Returns: `void` to make it obvious that `this` is mutated.
	*/
    void negate() @safe
    {
		static if (cow) { selfdupIfAliased(); }
        pragma(inline, true);
        _z._mp_size = -_z._mp_size; // fast C macro `mpz_neg` in gmp.h
    }

    /** Make `this` the absolute value of itself in-place.

		Returns: `void` to make it obvious that `this` is mutated.
	*/
    void absolute() @trusted
    {
        version(LDC) pragma(inline, true);
        if (isZero) { return; } // `__gmpz_abs` cannot handle default-constructed `this`
		static if (cow) { selfdupIfAliased(); }
        __gmpz_abs(_ptr, _ptr); version(ccc) { ++_ccc; }
    }
	alias absSelf = absolute;

    /** Make `this` the one's complement value of itself in-place.

		Returns: `void` to make it obvious that `this` is mutated.
	*/
    void onesComplementSelf() @trusted
    {
        version(LDC) pragma(inline, true);
        if (isZero) { return; } // `__gmpz_com` cannot handle default-constructed `this`
		static if (cow) { selfdupIfAliased(); }
        __gmpz_com(_ptr, _ptr); version(ccc) { ++_ccc; }
    }

    /** Make `this` the square root of itself in-place.

		Returns: `void` to make it obvious that `this` is mutated.
	*/
    void sqrtSelf() @trusted
    {
        version(LDC) pragma(inline, true);
        if (isZero) { return; } // `__gmpz_co` cannot handle default-constructed `this`
		static if (cow) { selfdupIfAliased(); }
        __gmpz_sqrt(_ptr, _ptr); version(ccc) { ++_ccc; }
    }

    /** Make `this` the `n`:th root of itself in-place.

		Returns: `true` if computation was exact.
	*/
    bool rootSelf(ulong n) @trusted
    {
        version(LDC) pragma(inline, true);
        if (isZero) { return true; } // `__gmpz_co` cannot handle default-constructed `this`
		static if (cow) { selfdupIfAliased(); }
        const status = __gmpz_root(_ptr, _ptr, n); version(ccc) { ++_ccc; }
		return status != 0;
    }

    /** Make `this` the `n`:th root of itself in-place along with its remainder returned.

		Returns: `rem` to the remainder, u-root ^^ `n`.
	*/
    _Z rootremSelf(ulong n) @trusted
    {
        version(LDC) pragma(inline, true);
        if (isZero) { return typeof(return).init; } // `__gmpz_co` cannot handle default-constructed `this`
		static if (cow) { selfdupIfAliased(); }
		_Z rem;
        __gmpz_rootrem(_ptr, rem._ptr, _ptr, n); version(ccc) { ++_ccc; }
		return move(rem);
    }

    /// Increase `this` by one.
    ref _Z opUnary(string s)() scope return @trusted
    if (s == "++")
    {
        version(LDC) pragma(inline, true);
		static if (cow) { selfdupIfAliased(); }
        if (isDefaultConstructed) // `__gmpz_add_ui` cannot handle default-constructed `this`
        {
            __gmpz_init_set_si(_ptr, 1);
        }
        else
        {
            __gmpz_add_ui(_ptr, _ptr, 1); version(ccc) { ++_ccc; }
        }
        return this;
    }

    /// Decrease `this` by one.
    ref _Z opUnary(string s)() scope return @trusted
    if (s == "--")
    {
        version(LDC) pragma(inline, true);
		static if (cow) { selfdupIfAliased(); }
        if (isDefaultConstructed) // `__gmpz_sub_ui` cannot handle default-constructed `this`
        {
            __gmpz_init_set_si(_ptr, -1);
        }
        else
        {
            __gmpz_sub_ui(_ptr, _ptr, 1); version(ccc) { ++_ccc; }
        }
        return this;
    }

    /// Get `base` raised to the power of `exp`.
    static typeof(this) pow(Base, Exp)(Base base, Exp exp) @trusted
    if (isIntegral!Base &&
        isIntegral!Exp)
    {
        version(LDC) pragma(inline, true);
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
            assert(exp >= 0, "Negative exponent"); // TODO: return mpq?
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

    /// Get number of digits in base `base`.
    size_t sizeInBase(uint base) const @trusted
    {
        pragma(inline, true);
        if (isZero)
        {
            return 1;
        }
        else
        {
            return __gmpz_sizeinbase(_ptr, base);
        }
    }

    /** Serialize `this` into an existing pre-allocated slice of words `words`,
		each word of type `Word`.

		It's format defined by:
		- `order`: the most significant word `first` or `last` for least significant first
		- `size` in bytes of each word
		- `endian` can be `bigEndian`, `littleEndian` or `host` default
		- the most significant `nails` bits of each word are unused and set to zero, this can be 0 to produce full words

		Returns: a (sub-)slice of `words` containing only the words produced.
	*/
    Word[] serialize(Word)(return scope Word[] words,
                           WordOrder order, size_t size, WordEndianess endian, size_t nails) const @trusted
    if (isUnsigned!Word)
    {
        assert(words, "Words is empty");

        size_t count;
        debug
        {
            const numb = 8 * size - nails;
            size_t items = (__gmpz_sizeinbase(_ptr, 2) + numb-1) / numb;
            assert(Word.sizeof * words.length >= items * size ,
                   "Words has not enough space pre-allocated");
        }

        int realOrder;
        final switch(order)
        {
            case WordOrder.mostSignificantWordFirst:  realOrder = 1;  break;
            case WordOrder.leastSignificantWordFirst: realOrder = -1; break;
        }

        int realEndian;
        final switch(endian)
        {
            case WordEndianess.littleEndian: realEndian = -1; break;
            case WordEndianess.bigEndian:    realEndian =  1; break;
            case WordEndianess.host:         realEndian =  0; break;
        }

        __gmpz_export(words.ptr, &count, realOrder, size, realEndian, nails, _ptr);
        return words[0 .. count];
    }


    /// Returns: `true` iff `this` fits in a `T`.
    bool fitsIn(T)() const @trusted
    if (isIntegral!T)
    {
        pragma(inline, true);
        if (isZero) { return true; } // default-constructed `this`
        static      if (is(T == ulong))  { return __gmpz_fits_ulong_p(_ptr) != 0; }
        else static if (is(T ==  long))  { return __gmpz_fits_slong_p(_ptr) != 0; }
        else static if (is(T ==  uint))  { return __gmpz_fits_uint_p(_ptr) != 0; }
        else static if (is(T ==   int))  { return __gmpz_fits_sint_p(_ptr) != 0; }
        else static if (is(T == ushort)) { return __gmpz_fits_ushort_p(_ptr) != 0; }
        else static if (is(T ==  short)) { return __gmpz_fits_sshort_p(_ptr) != 0; }
        else { static assert(false, "Unsupported type " ~ T.stringof); }
    }

    /** Get population count of `this`.

		If
		- `this` >= 0, number of 1 bits in the binary representation
	    - otherwise, ???
	*/
    @property mp_bitcnt_t populationCount() const @trusted
    {
        pragma(inline, true);
        if (isZero) { return 0; } // default-constructed `this`
        return __gmpz_popcount(this._ptr); // TODO: use core.bitop `popcnt` inline here instead?
    }
    alias countOnes = populationCount;

    /// Set bit at 0-offset index `bitIndex` (to one).
    void setBit(mp_bitcnt_t bitIndex) @trusted
    {
        pragma(inline, true);
		static if (cow) { selfdupIfAliased(); }
        __gmpz_setbit(_ptr, bitIndex);
    }

    /// Clear bit at 0-offset index `bitIndex` (to zero).
    void clearBit(mp_bitcnt_t bitIndex) @trusted
    {
        pragma(inline, true);
		static if (cow) { selfdupIfAliased(); }
        __gmpz_clrbit(_ptr, bitIndex);
    }

    /// Complement bit at 0-offset index `bitIndex` (to zero).
    void complementBit(mp_bitcnt_t bitIndex) @trusted
    {
        pragma(inline, true);
		static if (cow) { selfdupIfAliased(); }
        __gmpz_combit(_ptr, bitIndex);
    }

    /// Test/Get bit at 0-offset index `bitIndex`.
    bool testBit(mp_bitcnt_t bitIndex) const @trusted
    {
        pragma(inline, true);
        return __gmpz_tstbit(_ptr, bitIndex) != 0;
    }
    alias getBit = testBit;

    /** Check if `this` is default-constructed (in constrast to having been
        initialized via `__gmpz_...`-operations).

        Used to specially prepare avoid some `__gmpz_`-functions that cannot
        handle the case when the membere `_mp_alloc` is zero and, in turn,
        `_mp_d` is `null`.
     */
    @property bool isDefaultConstructed() const @safe
    {
        pragma(inline, true);
        return _z._mp_alloc == 0; // fast, actually enough to just test this
    }

    /// Check if `this` is zero (either via default-construction or `__gmpz_...`-operations).
    @property bool isZero() const @safe
    {
        pragma(inline, true);
        return _z._mp_size == 0; // fast
    }

    /// Check if `this` is odd.
    @property bool isOdd() const @safe
    {
        pragma(inline, true);
        return ((_z._mp_alloc != 0) && // this is needed for default (zero) initialized `__mpz_structs`
                ((_z._mp_size != 0) & cast(int)(_z._mp_d[0]))); // fast C macro `mpz_odd_p` in gmp.h
    }

    /// Check if `this` is odd.
    @property bool isEven() const @safe
    {
        pragma(inline, true);
        return !isOdd;            // fast C macro `mpz_even_p` in gmp.h
    }

    /// Check if `this` is negative.
    @property bool isNegative() const @safe
    {
        pragma(inline, true);
        return _z._mp_size < 0; // fast
    }

    /// Check if `this` is positive.
    @property bool isPositive() const @safe
    {
        pragma(inline, true);
        return !isNegative;     // fast
    }

	/** Check if `this` is a perfect power, i.e., if there exist integers A and
	 * B, with B>1, such that `this` equals A raised to the power B.
	 */
    @property bool isPerfectPower() const @trusted
    {
        pragma(inline, true);
		return __gmpz_perfect_power_p(_ptr) != 0;
    }

	/** Return non-zero if `this` is a perfect square, i.e., if the square root
	 * of op is an integer. Under this definition both 0 and 1 are considered to
	 * be perfect squares.
	 */
    @property bool isPerfectSquare() const @trusted
    {
        pragma(inline, true);
		return __gmpz_perfect_square_p(_ptr) != 0;
    }

    /** Returns: sign as either

      - -1 (`this` < 0),
	  -  0 (`this` == 0), or
	  - +1 (`this` > 0).
     */
    @property int sgn() const @safe
    {
        pragma(inline, true);
        return _z._mp_size < 0 ? -1 : _z._mp_size > 0; // fast C macro `mpz_sgn` in gmp.h
    }

    /// Number of significant `uint`s used for storing `this`.
    @property size_t uintLength() const
    {
        pragma(inline, true);
        assert(false, "TODO: use mpz_size");
    }

    /// Number of significant `ulong`s used for storing `this`.
    @property size_t uintLong() const
    {
        pragma(inline, true);
        assert(false, "TODO: use mpz_size");
    }

    /// Get number of limbs in internal representation.
    @property uint limbCount() const @safe
    {
        pragma(inline, true);
        return _integralAbs(_z._mp_size);
    }

private:

    /** Initialize internal struct. */
    void initialize() @system // cannot be called `init` as that will override builtin type property
    {
        pragma(inline, true);
        __gmpz_init(_ptr); version(ccc) { ++_ccc; }
    }

    /** Initialize internal struct if needed. */
    void assertInitialized() @system
    {
        pragma(inline, true);
        if (isDefaultConstructed)
        {
            __gmpz_init(_ptr); version(ccc) { ++_ccc; }
        }
    }

    /// Default conversion base.
    enum defaultBase = 10;

    /// Returns: evaluation of `this` expression which in this is a no-op.
    ref inout(_Z) eval() @safe inout scope return
    {
        pragma(inline, true);
        return this;
    }

    /// Type of limb in internal representation.
    alias Limb = __mp_limb_t;   // GNU MP alias

    /** Returns: limbs. */
    inout(Limb)[] _limbs() inout return @system
    {
        pragma(inline, true);
        // import std.math : abs;
        return _z._mp_d[0 .. limbCount];
    }

    /// Returns: pointer to internal C struct.
    inout(__mpz_struct)* _ptr() inout return @system
    {
        pragma(inline, true);
        return &_z;
    }

	static if (cow)
	{
		private __mpz_struct _z; // internal libgmp C struct
		private size_t _refCountCopies; ///< Number of copies.
	}
	else
	{
		private __mpz_struct _z; // internal libgmp C struct
	}

    version(ccc)
    {
        /** Number of calls made to `__gmpz`--functions that construct or changes
            this value. Used to verify correct lowering and evaluation of template
            expressions.

            For instance the `x` in `x = y + z` should be assigned only once inside
            a call to `mpz_add`.
        */
        @property size_t mutatingCallCount() const @safe { return _ccc; }
        size_t _ccc;  // C mutation call count. number of calls to C GMP function calls that mutate this object
    }

    /// @nogc-variant of `toStringz` with heap allocation of null-terminated C-string `stringz`.
    char* _allocStringzCopyOf(in char[] value) @nogc @trusted
    {
        import core.memory : pureMalloc;
        char* stringz = cast(char*)pureMalloc(value.length + 1); // maximum this many characters
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

    // qualified C memory managment
    static @trusted extern(C) // locally `@trusted`
    {
        pragma(mangle, "free")
        void qualifiedFree(void* ptr);
    }
}

/** Arbitrary precision integer (BigInt) with explicit copying via `.dup`.

	For copyable using copy-on-write automatic reference counting semantics, use
	`CopyableMpZ`.
*/
alias MpZ = _Z!(false);

/** Arbitrary precision integer (BigInt) with copy-on-write (CoW) automatic
	reference counting (ARC) API-compatible with `std.bigint`.
*/
alias CopyableMpZ = _Z!(true);

version(unittest) static assert(isMpZExpr!(MpZ));

version(benchmark)
@safe unittest
{
    import std.datetime.stopwatch : benchmark;
    bool odd;
    void test()
    {
        odd = (9.Z^^333333L).isOdd;
    }
    immutable results = benchmark!test(1);
    import std.stdio;
    writeln("Took:", results[0]);
}

pure:

/** Instantiator for `MpZ`. */
_Z!(cow) mpz(bool cow = true, Args...)(Args args) @safe
{
    version(LDC) pragma(inline, true);
    return typeof(return)(args);
}

/// Swap contents of `x` with contents of `y`.
void swap(bool cow)(ref _Z!(cow) x, ref _Z!(cow) y) nothrow
{
    pragma(inline, true);
    import std.algorithm.mutation : swap;
    swap(x, y); // x.swap(y);
}

/// Get `x` as a `string` in decimal base.
string toDecimalString(bool cow)(auto ref scope const _Z!(cow) x) nothrow // for `std.bigint.BigInt` compatibility
{
    version(LDC) pragma(inline, true);
    return x.toString(10);
}

/// Get `x` as a uppercased `string` in hexadecimal base without any base prefix (0x).
string toHexadecimalString(bool cow)(auto ref scope const _Z!(cow) x) nothrow
{
    version(LDC) pragma(inline, true);
    return x.toString(16, true);
}

/// For `std.bigint.BigInt` compatibility.
alias toHex = toHexadecimalString;

/// Get the absolute value of `x` converted to the corresponding unsigned type.
Unsigned!T absUnsign(T, bool cow)(auto ref scope const _Z!(cow) x) nothrow // for `std.bigint.BigInt` compatibility
if (isIntegral!T)
{
    version(LDC) pragma(inline, true);
    return _integralAbs(cast(T)x);
}

/** Get sum of `x` and `y` (`x` + `y`).
 */
_Z!(cow) add(bool cow)(auto ref scope const _Z!(cow) x, auto ref scope const _Z!(cow) y) nothrow @trusted
{
    version(LDC) pragma(inline, true);
    static if (!__traits(isRef, x) || // r-value `x`
               !__traits(isRef, y))   // r-value `y`
    {
        typeof(return)* zp = null;        // reuse: will point to either `x` or `y`
        static if (!__traits(isRef, x) && // r-value `x`
                   !__traits(isRef, y))   // r-value `y`
        {
            if (x.limbCount > y.limbCount) // larger r-value `x`
            {
                zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
            }
            else                    // larger r-value `y`
            {
                zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
            }
        }
        else static if (!__traits(isRef, x)) // r-value `x`
        {
            zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        }
        else static if (!__traits(isRef, y)) // r-value `y`
        {
            zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
        }
		else
		{
			static assert(0);
		}
		__gmpz_add(zp._ptr, x._ptr, y._ptr);
		static if (cow) { zp.selfdupIfAliased(); }
        version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
    else                        // l-value `x` and `y`, no reuse in output
    {
        typeof(return) z = null;
        __gmpz_add(z._ptr, x._ptr, y._ptr); version(ccc) ++z._ccc;
        return z;
    }
}

///
@safe nothrow @nogc unittest
{
    const Z x = Z(2)^^100;
    const Z y = 12;
    assert(add(x, Z(12)) ==       // l-value, r-value
           add(Z(12), x));        // r-value, l-value
    assert(add(x, y) ==           // l-value, l-value
           add(Z(2)^^100, Z(12)));  // r-value, r-value
    assert(add(Z(12), Z(2)^^100) == // r-value, r-value
           add(Z(2)^^100, Z(12)));  // r-value, r-value
}

/** Get difference of `x` and `y` (`x` - `y`).
 */
_Z!(cow) sub(bool cow)(auto ref scope const _Z!(cow) x, auto ref scope const _Z!(cow) y) nothrow @trusted
{
    version(LDC) pragma(inline, true);
    static if (!__traits(isRef, x) || // r-value `x`
               !__traits(isRef, y))   // r-value `y`
    {
        typeof(return)* zp = null;        // reuse: will point to either `x` or `y`
        static if (!__traits(isRef, x) && // r-value `x`
                   !__traits(isRef, y))   // r-value `y`
        {
            if (x.limbCount > y.limbCount) // larger r-value `x`
            {
                zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
            }
            else                    // larger r-value `y`
            {
                zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
            }
        }
        else static if (!__traits(isRef, x)) // r-value `x`
        {
            zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        }
        else static if (!__traits(isRef, y)) // r-value `y`
        {
            zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
        }
		else
		{
			static assert(0);
		}
		static if (cow) { zp.selfdupIfAliased(); }
		__gmpz_sub(zp._ptr, x._ptr, y._ptr);
        version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
    else                        // l-value `x` and `y`, no reuse in output
    {
        typeof(return) z = null;
        __gmpz_sub(z._ptr, x._ptr, y._ptr); version(ccc) ++z._ccc;
        return z;
    }
}

///
@safe nothrow @nogc unittest
{
    Z x = 2.Z^^100;
    Z y = 12;
    assert(sub(x, Z(12)) ==       // l-value, r-value
           -sub(Z(12), x));       // r-value, l-value
    assert(sub(x, y) ==           // l-value, l-value
           sub(2.Z^^100, 12.Z));  // r-value, r-value
    assert(sub(12.Z, 2.Z^^100) == // r-value, r-value
           -sub(2.Z^^100, 12.Z)); // r-value, r-value
}

/** Get product of `x` and `y` (`x` + `y`).
 */
_Z!(cow) mul(bool cow)(auto ref scope const _Z!(cow) x, auto ref scope const _Z!(cow) y) nothrow @trusted
{
    version(LDC) pragma(inline, true);
    static if (!__traits(isRef, x) || // r-value `x`
               !__traits(isRef, y))   // r-value `y`
    {
        typeof(return)* zp = null;        // reuse: will point to either `x` or `y`
        static if (!__traits(isRef, x) && // r-value `x`
                   !__traits(isRef, y))   // r-value `y`
        {
            if (x.limbCount > y.limbCount) // larger r-value `x`
            {
                zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
            }
            else                    // larger r-value `y`
            {
                zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
            }
        }
        else static if (!__traits(isRef, x)) // r-value `x`
        {
            zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        }
        else static if (!__traits(isRef, y)) // r-value `y`
        {
            zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
        }
		else
		{
			static assert(0);
		}
		static if (cow) { zp.selfdupIfAliased(); }
		__gmpz_mul(zp._ptr, x._ptr, y._ptr);
        version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
    else                        // l-value `x` and `y`, no reuse in output
    {
        typeof(return) z = null;
        __gmpz_mul(z._ptr, x._ptr, y._ptr); version(ccc) ++z._ccc;
        return z;
    }
}

///
@safe nothrow @nogc unittest
{
    Z x = 2.Z^^100;
    Z y = 12;
    assert(mul(x, Z(12)) ==       // l-value, r-value
           mul(Z(12), x));        // r-value, l-value
    assert(mul(x, y) ==           // l-value, l-value
           mul(2.Z^^100, 12.Z));  // r-value, r-value
    assert(mul(12.Z, 2.Z^^100) == // r-value, r-value
           mul(2.Z^^100, 12.Z));  // r-value, r-value
}

/** Get absolute value of `x`.

	Written as a free function instead of `MpZ`-member because `__traits(isRef, this)` cannot be used.
*/
_Z!(cow) abs(bool cow)(auto ref scope const _Z!(cow) x) @trusted nothrow @nogc
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
        __gmpz_abs(y._ptr, x._ptr); version(ccc) ++y._ccc;
        return y;
    }
    else                        // r-value `x`
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
		static if (cow) { zp.selfdupIfAliased(); }
        zp.absolute(); version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
}

/** Get one's complement of `x`.

	Written as a free function instead of `MpZ`-member because `__traits(isRef, this)` cannot be used.
*/
_Z!(cow) onesComplement(bool cow)(auto ref scope const _Z!(cow) x) @trusted nothrow @nogc
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
        __gmpz_com(y._ptr, x._ptr); version(ccc) ++y._ccc;
        return y;
    }
    else                        // r-value `x`
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        zp.onesComplementSelf(); version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
}
alias com = onesComplement;

/** Get truncated integer part of the square root of `x`.

	Written as a free function instead of `MpZ`-member because `__traits(isRef, this)` cannot be used.

	See: https://gmplib.org/manual/Integer-Roots
*/
_Z!(cow) sqrt(bool cow)(auto ref scope const _Z!(cow) x) @trusted nothrow @nogc
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
        __gmpz_sqrt(y._ptr, x._ptr); version(ccc) ++y._ccc;
        return y;
    }
    else                        // r-value `x`
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        zp.sqrtSelf(); version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
}

/** Get truncated integer part of the `n`:th root of `x`.

	See: https://gmplib.org/manual/Integer-Roots
*/
_Z!(cow) root(bool cow)(auto ref scope const _Z!(cow) x, ulong n, out bool isExact) @trusted nothrow @nogc
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
        const status = __gmpz_root(y._ptr, x._ptr, n); version(ccc) ++y._ccc;
		isExact = status != 0;
        return y;
    }
    else                        // r-value `x`
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        isExact = zp.rootSelf(n); version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
}

_Z!(cow) rootrem(bool cow)(auto ref scope const _Z!(cow) x, ulong n, out scope _Z!(cow) rem) @trusted nothrow @nogc
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
        __gmpz_rootrem(y._ptr, rem._ptr, x._ptr, n); version(ccc) ++y._ccc;
        return y;
    }
    else                        // r-value `x`
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        rem = zp.rootremSelf(n); version(ccc) ++zp._ccc;
		return move(*zp);
    }
}

/// Comparison of the absolute values of `x` and `y`.
int cmpabs(bool cow)(auto ref scope const _Z!(cow) x, auto ref scope const _Z!(cow) y) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    return __gmpz_cmpabs(x._ptr, y._ptr);
}
/// ditto
int cmpabs(bool cow)(auto ref scope const _Z!(cow) x, double y) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    return __gmpz_cmpabs_d(x._ptr, y);
}
/// ditto
int cmpabs(bool cow)(auto ref scope const _Z!(cow) x, ulong y) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    return __gmpz_cmpabs_ui(x._ptr, y);
}

/** Get next prime greater than `x`.

	Written as a free function instead of `MpZ`-member because `__traits(isRef, this)` cannot be used.
*/
_Z!(cow) nextPrime(bool cow)(auto ref scope const _Z!(cow) x) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) y = null; // must use temporary
		static if (cow) { y.selfdupIfAliased(); }
        __gmpz_nextprime(y._ptr, x._ptr); version(ccc) ++y._ccc;
        return y;
    }
    else                        // r-value `x`
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
		static if (cow) { zp.selfdupIfAliased(); }
        __gmpz_nextprime(zp._ptr, x._ptr); version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
}

/// Get greatest common divisor (gcd) of `x` and `y`.
_Z!(cow) gcd(bool cow)(auto ref scope const _Z!(cow) x, auto ref scope const _Z!(cow) y) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    static if (!__traits(isRef, x) || // r-value `x`
               !__traits(isRef, y))   // r-value `y`
    {
        typeof(return)* zp = null;        // reuse: will point to either `x` or `y`
        static if (!__traits(isRef, x) && // r-value `x`
                   !__traits(isRef, y))   // r-value `y`
        {
            if (x.limbCount > y.limbCount) // larger r-value `x`
            {
                zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
            }
            else                    // larger r-value `y`
            {
                zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
            }
        }
        else static if (!__traits(isRef, x)) // r-value `x`
        {
            zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        }
        else static if (!__traits(isRef, y)) // r-value `y`
        {
            zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
        }
		else
		{
			static assert(0);
		}
		static if (cow) { zp.selfdupIfAliased(); }
		__gmpz_gcd(zp._ptr, x._ptr, y._ptr);
        version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
    else                        // l-value `x` and `y`, no reuse in output
    {
        typeof(return) z = null;
        __gmpz_gcd(z._ptr, x._ptr, y._ptr); version(ccc) ++z._ccc;
        return z;
    }
}
/// ditto
_Z!(cow) gcd(bool cow)(auto ref scope const _Z!(cow) x, ulong y) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) z = null;
        const z_ui = __gmpz_gcd_ui(z._ptr, x._ptr, y); version(ccc) ++z._ccc;
        return z;
    }
    else
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
		static if (cow) { zp.selfdupIfAliased(); }
		const z_ui = __gmpz_gcd_ui(zp._ptr, x._ptr, y); version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
}

/// Get least common multiple (lcm) of `x` and `y`.
_Z!(cow) lcm(bool cow)(auto ref scope const _Z!(cow) x, auto ref scope const _Z!(cow) y) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    static if (!__traits(isRef, x) || // r-value `x`
               !__traits(isRef, y))   // r-value `y`
    {
        typeof(return)* zp = null;        // reuse: will point to either `x` or `y`
        static if (!__traits(isRef, x) && // r-value `x`
                   !__traits(isRef, y))   // r-value `y`
        {
            if (x.limbCount > y.limbCount) // larger r-value `x`
            {
                zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
            }
            else                    // larger r-value `y`
            {
                zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
            }
        }
        else static if (!__traits(isRef, x)) // r-value `x`
        {
            zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        }
        else static if (!__traits(isRef, y)) // r-value `y`
        {
            zp = (cast(typeof(return)*)(&y)); // @trusted because `MpZ` has no aliased indirections
        }
		else
		{
			static assert(0);
		}
		static if (cow) { zp.selfdupIfAliased(); }
		__gmpz_lcm(zp._ptr, x._ptr, y._ptr);
        version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
    else                        // l-value `x` and `y`, no reuse in output
    {
        typeof(return) z = null;
        __gmpz_lcm(z._ptr, x._ptr, y._ptr); version(ccc) ++z._ccc;
        return z;
    }
}
/// ditto
_Z!(cow) lcm(bool cow)(auto ref scope const _Z!(cow) x, ulong y) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    static if (__traits(isRef, x)) // l-value `x`
    {
        typeof(return) z = null;
        __gmpz_lcm_ui(z._ptr, x._ptr, y);
        return z;
    }
    else
    {
        typeof(return)* zp = (cast(typeof(return)*)(&x)); // @trusted because `MpZ` has no aliased indirections
        static if (cow) { zp.selfdupIfAliased(); }
		__gmpz_lcm_ui(zp._ptr, x._ptr, y); version(ccc) ++zp._ccc;
        return move(*zp);    // TODO: shouldn't have to call `move` here
    }
}

/** Get `base` ^^ `exp` (modulo `mod`).

	Parameter `exp` must be positive.
*/
_Z!(cow) powm(bool cow)(auto ref scope const _Z!(cow) base, auto ref scope const _Z!(cow) exp, auto ref scope const _Z!(cow) mod) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    assert(mod != 0, "Zero modulus");
    typeof(return) y = 0; // result, TODO: reuse `exp` or `mod` if any is an r-value
    assert(exp >= 0, "Negative exponent");
	static if (cow) { y.selfdupIfAliased(); }
    __gmpz_powm(y._ptr, base._ptr, exp._ptr, mod._ptr); version(ccc) ++y._ccc;
    return y;
}
/// ditto
_Z!(cow) powm(bool cow)(auto ref scope const _Z!(cow) base, ulong exp, auto ref scope const _Z!(cow) mod) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    assert(mod != 0, "Zero modulus");
    typeof(return) y = 0;       // result, TODO: reuse `exp` or `mod` if any is an r-value
	static if (cow) { y.selfdupIfAliased(); }
	__gmpz_powm_ui(y._ptr, base._ptr, exp, mod._ptr); version(ccc) ++y._ccc;
    return y;
}

/// For `std.bigint.BigInt` compatibility.
alias powmod = powm;

/** Get `base` ^^ `-1` (modulo `mod`).

	Parameter `mod` must be positive.
*/
_Z!(cow) invert(bool cow)(auto ref scope const _Z!(cow) base, auto ref scope const _Z!(cow) mod) nothrow @nogc @trusted
{
    version(LDC) pragma(inline, true);
    assert(base != 0, "Zero base");
    assert(mod != 0, "Zero modulus");
    static if (!__traits(isRef, base)) // r-value `base`
    {
        typeof(return)* mut_base = (cast(typeof(return)*)(&base)); // @trusted because `MpZ` has no aliased indirections
        static if (cow) { mut_base.selfdupIfAliased(); }
		auto success = __gmpz_invert(mut_base._ptr, base._ptr, mod._ptr); version(ccc) ++y._ccc;
        assert(success >= 0, "Cannot invert");
        return move(*mut_base);    // TODO: shouldn't have to call `move` here
    }
    else static if (!__traits(isRef, mod)) // r-value `mod`
    {
        typeof(return)* mut_mod = (cast(typeof(return)*)(&mod)); // @trusted because `MpZ` has no aliased indirections
        static if (cow) { mut_mod.selfdupIfAliased(); }
		auto success = __gmpz_invert(mut_mod._ptr, base._ptr, mod._ptr); version(ccc) ++y._ccc;
        assert(success >= 0, "Cannot invert");
        return move(*mut_mod);  // TODO: shouldn't have to call `move` here
    }
    else                        // l-value `base` and l-value `mod`
    {
        typeof(return) y = 0; // result, TODO: reuse `exp` or `mod` if any is an r-value
        static if (cow) { y.selfdupIfAliased(); }
		auto success = __gmpz_invert(y._ptr, base._ptr, mod._ptr); version(ccc) ++y._ccc;
        assert(success >= 0, "Cannot invert");
        return y;
    }
}

/// default construction
@safe nothrow @nogc unittest
{
    Z x = null;
    Z y = null;
    assert(x == y);
    assert(x is y);             // TODO: is this correct behaviour?
    x = y;
    y = x;

    Z z = 42;
    x = z;

    assert(Z.init.dup == Z.init);
    assert(Z.init.dup == Z.init.dup);

    assert(Z.init.dup !is Z.init);
    assert(Z.init.dup !is Z.init.dup);

    Z w = null;
    w = 42;
}

/// @nogc to ASCII generation
@trusted nothrow unittest
{
    import core.memory : pureFree;
    Z w = null;
    auto chars = w.toChars;
	scope(exit) pureFree(chars.ptr);
    assert(chars == `0`);
}

/// operate on default-constructed instances
@safe nothrow unittest
{
    Z w;

    // should be zeroed
    assert(w._z._mp_alloc == 0);
    assert(w._z._mp_size == 0);
    assert(w._z._mp_d is null);

    assert(w == 0);
    assert(w == 0L);
    assert(w == 0UL);
    assert(w == 0.0f);
    assert(w == 0.0);

    assert(w.toString == `0`);
    assert(w.toHash == 0);
    assert(w.sizeInBase(10) == 1);
    assert(w.countOnes == 0);
    assert(w.isZero);
    assert(-w == w);
    assert(abs(w) == w);

    assert(w.fitsIn!ulong);
    assert(w.fitsIn!long);
    assert(w.fitsIn!uint);
    assert(w.fitsIn!int);
    assert(w.fitsIn!ushort);
    assert(w.fitsIn!short);

    assert(!w.isOdd);
    assert(w.isEven);
    assert(!w.isNegative);
    assert(w.isPositive);
    assert(w.sgn == 0);

    w.negate();
    assert(w is Z.init);        // should be unchanged
    w.negate();
    assert(w is Z.init);        // should be unchanged

    w = -w;
    assert(w is Z.init);        // should be unchanged

    w = +w;
    assert(w is Z.init);        // should be unchanged

    w.absolute();
    assert(w is Z.init);        // should be unchanged

    w.onesComplementSelf();
    assert(w is Z.init);        // should be unchanged
    w.onesComplementSelf();
    assert(w is Z.init);        // should be unchanged

	assert(16.Z.sqrt == 4);
	{ bool isExact; assert(16.Z.root(2, isExact) == 4 && isExact); }
	{ bool isExact; assert(17.Z.root(2, isExact) == 4 && !isExact); }
	{ bool isExact; assert(27.Z.root(3, isExact) == 3 && isExact); }
	{ bool isExact; assert(28.Z.root(3, isExact) == 3 && !isExact); }

	foreach (const ui; 16 .. 20)
	{
		{
			Z u = ui;
			Z rem;
			const r = u.rootrem(2, rem); // l-value first-parameter
			assert(r == 4);
			assert(rem == ui - 16);
		}
		{
			Z rem;
			const r = ui.Z.rootrem(2, rem); // r-value first-parameter
			assert(r == 4);
			assert(rem == ui - 16);
		}
	}

    assert(w^^10 == 0);
    assert(w^^0 == 1);          // TODO: correct?

    // change it and check its contents
    w = 42;
    assert(w.toString == `42`);
    assert(w.toHash != 0);      // should at least be non-zero

    {
        Z p;
        p.setBit(5);
        assert(p == 32);
    }
    {
        Z p;
        p.clearBit(5);
        assert(p == 0);
    }
    {
        Z p;
        p.complementBit(5);
        assert(p == 32);
    }
    {
        Z p;
        assert(p.testBit(5) == 0);
    }
    {
        Z p;
        ++p;
        assert(p == 1);
    }
    {
        Z p;
        --p;
        assert(p == -1);
    }

    assert(Z.init + Z.init == Z.init);
    assert(Z.init - Z.init == Z.init);
    assert(Z.init * Z.init == Z.init);

    assert(Z.init + 1 == 1);
    assert(Z.init - 1 == -1);

    assert(Z.init * 1 == 0);
    assert(Z.init / 1 == 0);
    assert(Z.init ^^ 1 == Z.init);
    assert(Z.init ^^ 0 == 1);

    assert(1 + Z.init == 1);
    assert(1 - Z.init == 1);
    assert(1 * Z.init == 0);
}

/// null construction
@safe nothrow @nogc unittest
{
    Z x = null;
    Z y = null;
    assert(x == y);
    assert(x is y);    // TODO: this behaviour recently changed. is this correct?
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

///
@nogc unittest
{
    ubyte[int.sizeof] storage;
    ubyte[1] expected = [2];
    assert(storage[0] == 0);
    auto storage2 =  2.Z.serialize(storage, WordOrder.mostSignificantWordFirst, 1, WordEndianess.littleEndian, 0);
    assert(storage2.ptr == storage.ptr);
    assert(storage2 == expected);
}

unittest
{
    ubyte[int.sizeof] storage;
    ubyte[1] expected = [2];
    ubyte[] storage2 = 2.Z.serialize!(ubyte)(WordOrder.mostSignificantWordFirst, WordEndianess.littleEndian, 0);
    assert(storage2 == expected);
}

///
unittest
{
    auto prime = 33391.Z;
    for (int i = 0; i < 20; ++i)
    {
        prime = nextPrime(prime);
        for (auto order = WordOrder.min; order < WordOrder.max; ++order)
        {
            for (auto endianess = WordEndianess.min; endianess < WordEndianess.max; ++endianess)
            {
                ubyte[] data = prime.serialize!(ubyte)(order, endianess, 0);
                auto samePrime = Z(data, order, 1, endianess, 0);
                assert(prime == samePrime);
            }
        }
    }
}

/// opBinary with r-value right-hand-side
@safe @nogc unittest
{
    Z a = 42;
    {
        Z b = a + 1.Z;              // r-value `rhs`
        version(ccc) assert(b.mutatingCallCount == 2);
        assert(b == 43);
    }

    {
        Z b = a - 1.Z;              // r-value `rhs`
        version(ccc) assert(b.mutatingCallCount == 2);
        assert(b == 41);
    }

    {
        Z b = a * 2.Z;              // r-value `rhs`
        version(ccc) assert(b.mutatingCallCount == 2);
        assert(b == 84);
    }

    {
        Z b = a / 2.Z;              // r-value `rhs`
        version(ccc) assert(b.mutatingCallCount == 2);
        assert(b == 21);
    }

    {
        Z b = a % 10.Z;              // r-value `rhs`
        version(ccc) assert(b.mutatingCallCount == 2);
        assert(b == 2);
    }
}

///
@safe unittest
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

    w -= 100.Z;
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

    w /= 2.Z;
    assert(w == 21);

    w /= -2.Z;
    assert(w == -10);

    w *= -2.Z;
    assert(w == 20);

    w %= 3.Z;
    assert(w == 2);

    w *= -1.Z;
    assert(w == -2);

    w /= -1.Z;
    assert(w == 2);

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
    assert(Z.fromBinaryString(`11`) == 3);
    assert(Z.fromBinaryString(`0b11`) == 3);
    assert(Z.fromBinaryString(`0B11`) == 3);

    // octal

    assert(`07`.Z == 7);
    assert(`010`.Z == 8);

    // hexadecimal

    assert(`0x10`.Z == 16);
    assert(`0X10`.Z == 16);
    assert(Z(`10`, 16) == 16);
    assert(Z.fromHexString(`10`) == 16);
    assert(Z.fromHexString(`0x10`) == 16);
    assert(Z.fromHexString(`0X10`) == 16);
    assert(Z.fromHexString!(`10`) == 16);
    assert(Z.fromHexString!(`0x10`) == 16);
    assert(Z.fromHexString!(`0X10`) == 16);

    // decimal

    assert(`101`.Z == 101);
    assert(`101`.Z == 101);

    const ic = 101UL.Z;

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

    n.onesComplementSelf();
    assert(n == -43);
    assert(onesComplement(n) == 42);
    assert(onesComplement(-43.Z) == 42);
    assert(com(n) == 42);
    assert(com(-43.Z) == 42);

	/*TODO: figure out why this call doesn’t trigger a warn-unused warning when
	 * the functions below returning `S` above does */
	onesComplement(42.Z);
	static if (false) {
		struct S(T) {
			T x;
			T* xp;						// this prevents diagnostics
		}
		static S!int f()() @safe pure nothrow @nogc { return typeof(return).init; }
		static S!int g()(scope S!int x) @safe pure nothrow @nogc { return x; }
		static const(S!int) h1()(scope const(S!int) x) @safe pure nothrow @nogc { return x; }
		static S!int h2()(scope const(S!int) x) @safe pure nothrow @nogc { return typeof(return).init; }
		g(S!int(32));
		f();
		h1(S!int(32));
		h2(S!int(32));
	}

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
    assert( 28.Z  % -3.Z == 1);  // TODO: should be -1
    assert(-28.Z  %  3.Z == -1);  // TODO: should be 1
    assert( 28  % -3.Z == 1);      // TODO: should be -1
    assert(-28  %  3.Z == -1);     // TODO: should be 1

    // modulo/remainder

    const one = 1.Z;
    const two = 2.Z;
    const three = 3.Z;
    const four = 4.Z;
    const five = 5.Z;
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

    // modular multiplicative inverse
    assert(3.Z.invert(26.Z) == 9.Z); // r-value `base`
    assert(3.Z.invert(-26.Z) == 9.Z); // r-value `base`
    {
        auto base = 3.Z;
        assert(base.invert(26.Z) == 9.Z); // l-value `base` and r-value `mod`
    }
    {
        auto base = 3.Z;
        auto mod = 26.Z;
        assert(base.invert(mod) == 9.Z); // l-value `base` and l-value `mod
    }

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

	foreach (const p; 1 .. 10)
	{
		assert((p^^2).Z.isPerfectSquare);
		assert(!(p^^2 + 1).Z.isPerfectSquare);
		foreach (const q; 2 .. 5)
		{
			import std.stdio;
			debug writeln("p:", p, " q:", q);
			assert((p^^q).Z.isPerfectPower);
		}
	}

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

    assert(1.Z.populationCount == 1);
    assert(2.Z.populationCount == 1);
    assert(3.Z.populationCount == 2);
    assert(4.Z.populationCount == 1);
    assert(5.Z.populationCount == 2);
    assert(6.Z.populationCount == 2);
    assert(7.Z.populationCount == 3);

    // TODO
    // {
    //     Z g = null;
    //     assert(!b.testBit(0));
    //     g.setBit(0);
    //     assert(b.testBit(0));
    //     g.clearBit(0);
    //     assert(!b.testBit(0));
    // }

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

    // limb count

    assert(0.Z.limbCount == 0);
    assert(1.Z.limbCount == 1);
    assert(2.Z.limbCount == 1);

    assert(Z.pow(2UL, 32UL).limbCount == 1);

    assert(Z.pow(2UL, 63UL).limbCount == 1);
    assert(Z.pow(2UL, 63UL + 1).limbCount == 2);

    assert(Z.pow(2UL, 127UL).limbCount == 2);
    assert(Z.pow(2UL, 127UL + 1).limbCount == 3);

    assert(Z.pow(2UL, 255UL).limbCount == 4);
    assert(Z.pow(2UL, 255UL + 1).limbCount == 5);
}

/// generators
@safe @nogc unittest
{
    assert(Z.mersennePrime(15) == 2^^15 - 1);
    assert(Z.mersennePrime(15UL) == 2^^15 - 1);
}

/// left shift
@safe @nogc unittest
{
    assert(1.Z << 1.Z == 2^^1);
    assert(1.Z << 2.Z == 2^^2);
    assert(1.Z << 32.Z == 2UL^^32);
    assert(1.Z << 63.Z == 2UL^^63);

    assert(1.Z << 1U == 2^^1);
    assert(1.Z << 2U == 2^^2);
    assert(1.Z << 32U == 2UL^^32);
    assert(1.Z << 63U == 2UL^^63);
}

/// verify compliance with Phobos' `BigInt`
@safe unittest
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
        const h = g / b;
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
        // TODO: static assert(is(typeof(x % s)  == short));
        // TODO: static assert(is(typeof(x % b)  == byte));

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

/// Fermats Little Theorem
@safe @nogc unittest
{
    version (unittestLong)
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

    version (unittestLong)
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
                        __gmpz_rootrem(r1._ptr, rem._ptr, r1._ptr, POWER); version(ccc) ++r1._ccc; // TODO: replace with call to D wrapper
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
private struct AddExpr(bool cow)
{
    _Z!(cow) e1;				// first term
    _Z!(cow) e2;				// second term
    _Z!(cow) eval() const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref _Z!(cow) y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_add(y._ptr, e1.eval()._ptr, e2.eval()._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(AddExpr!(true)));

@safe @nogc unittest
{
    assert(AddExpr!(false)(3.Z, 4.Z).eval() == 3 + 4);

    const Z x = AddExpr!(false)(3.Z, 4.Z);
    version(ccc) assert(x.mutatingCallCount == 1); // lower to `mpz_add`
    assert(x == 7);

    Z y = null;
    y = AddExpr!(false)(3.Z, 4.Z);
    version(ccc) assert(y.mutatingCallCount == 2); // lowers to `mpz_init`, `mpz_add`
    assert(y == 7);

}

/// `MpZ`-`MpZ` subtraction expression.
private struct SubExpr(bool cow)
{
    _Z!(cow) e1;                      // first term
    _Z!(cow) e2;                      // second term
    _Z!(cow) eval() const nothrow @nogc @trusted   // TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref _Z!(cow) y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_sub(y._ptr, e1.eval()._ptr, e2.eval()._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(SubExpr!(false)));

@safe @nogc unittest
{
    assert(SubExpr!(false)(3.Z, 4.Z).eval() == 3 - 4);
    const Z x = SubExpr!(false)(3.Z, 4.Z);
    version(ccc) assert(x.mutatingCallCount == 1); // lowers to `mpz_sub`
    assert(x == -1);
}

/// `MpZ`-`MpZ` multiplication expression.
private struct MulExpr(bool cow)
{
    _Z!(cow) e1;				// first factor
    _Z!(cow) e2;				// second factor
    _Z!(cow) eval() const nothrow @nogc @trusted   // TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref _Z!(cow) y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_mul(y._ptr, e1.eval()._ptr, e2.eval()._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(MulExpr!(false)));

@safe @nogc unittest
{
    assert(MulExpr!(false)(3.Z, 4.Z).eval() == 3 * 4);

    const Z x = MulExpr!(false)(3.Z, 4.Z);
    assert(x == 12);
    version(ccc) assert(x.mutatingCallCount == 1); // lowers to `mpz_mul`
}

/// `MpZ`-`MpZ` division expression.
private struct DivExpr(bool cow)
{
    _Z!(cow) e1;				// divisor
    _Z!(cow) e2;				// dividend
    _Z!(cow) eval() const nothrow @nogc @trusted	// TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref _Z!(cow) y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_tdiv_q(y._ptr, e1.eval()._ptr, e2.eval()._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(DivExpr!(false)));

@safe @nogc unittest
{
    assert(DivExpr!(false)(27.Z, 3.Z).eval() == 27 / 3);
    assert(DivExpr!(false)(28.Z, 3.Z).eval() == 28 / 3);
    assert(DivExpr!(false)(29.Z, 3.Z).eval() == 29 / 3);
    assert(DivExpr!(false)(30.Z, 3.Z).eval() == 30 / 3);

    const Z x = DivExpr!(false)(28.Z, 3.Z);
    assert(x == 9);
    version(ccc) assert(x.mutatingCallCount == 1); // lowers to `mpz_tdiv_q`
}

/// `MpZ`-`MpZ` modulus expression.
private struct ModExpr(bool cow)
{
    _Z!(cow) e1;				// divisor
    _Z!(cow) e2;				// dividend
    _Z!(cow) eval() const nothrow @nogc @trusted   // TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref _Z!(cow) y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_tdiv_r(y._ptr, e1.eval()._ptr, e2.eval()._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(ModExpr!(false)));

@safe @nogc unittest
{
    assert(ModExpr!(false)(27.Z, 3.Z).eval() == 27 % 3);
    assert(ModExpr!(false)(28.Z, 3.Z).eval() == 28 % 3);
    assert(ModExpr!(false)(29.Z, 3.Z).eval() == 29 % 3);
    assert(ModExpr!(false)(30.Z, 3.Z).eval() == 30 % 3);

    const Z x = ModExpr!(false)(29.Z, 3.Z);
    assert(x == 2);
    version(ccc) assert(x.mutatingCallCount == 1); // lowers to `mpz_tdiv_r`
}

/// `MpZ`-`ulong` power expression.
private struct PowUExpr(P, Q)
if (isMpZExpr!P &&
    isUnsigned!Q)
{
    P e1;                       // base
    Q e2;                       // exponent
    MpZ eval() const nothrow @nogc @trusted   // TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref MpZ y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_pow_ui(y._ptr, e1.eval()._ptr, e2); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(PowUExpr!(MpZ, ulong)));

@safe @nogc unittest
{
    assert(PowUExpr!(Z, ulong)(3.Z, 3).eval() == 3^^3);
}

/// `MpZ`-`ulong`-`MpZ` power-modulo expression.
private struct PowMUExpr(P, Q, M)
if (isMpZExpr!P &&
    isUnsigned!Q &&
    isMpZExpr!M)
{
    P base;                     // base
    Q exp;                      // exponent
    M mod;                      // modulo
    MpZ eval() const nothrow @nogc @trusted   // TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref MpZ y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_powm_ui(y._ptr, base.eval()._ptr, exp, mod._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(PowMUExpr!(MpZ, ulong, MpZ)));

@safe @nogc unittest
{
    assert(PowMUExpr!(Z, ulong, Z)(3.Z, 3, 20.Z).eval() == 3^^3 % 20);
}

/// `MpZ` negation expression.
private struct NegExpr(bool cow)
{
    _Z!(cow) e1;
    _Z!(cow) eval() const nothrow @nogc @trusted   // TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref _Z!(cow) y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_neg(y._ptr, e1.eval()._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(NegExpr!(false)));

@safe @nogc unittest
{
    assert(NegExpr!(false)(27.Z).eval() == -27);
    const Z x = NegExpr!(false)(27.Z);
    version(ccc) assert(x.mutatingCallCount == 1); // lowers to `mpz_neg`
    assert(x == -27);
}

/** `MpZ` square root expression.

	See: https://gmplib.org/manual/Integer-Roots
 */
private struct SqrtExpr(bool cow)
{
    _Z!(cow) e1;
    _Z!(cow) eval() const nothrow @nogc @trusted   // TODO: move to common place
    {
        version(LDC) pragma(inline, true);
        typeof(return) y = null;
        evalTo(y);
        return y;
    }
    void evalTo(ref _Z!(cow) y) const nothrow @nogc @trusted
    {
        version(LDC) pragma(inline, true);
        __gmpz_sqrt(y._ptr, e1.eval()._ptr); version(ccc) ++y._ccc;
    }
}
version(unittest) static assert(isMpZExpr!(SqrtExpr!(false)));

@safe @nogc unittest
{
	foreach (const n; 16 .. 25)
		assert(SqrtExpr!(false)(n.Z).eval() == 4);
	assert(SqrtExpr!(false)(25.Z).eval() == 5);
}

// Copied from `std.numeric` to prevent unnecessary Phobos deps.
private T _integralAbs(T)(T x)
if (isIntegral!T)
{
    pragma(inline, true);
    return x >= 0 ? x : -x;
}

/// as hash table key
@safe unittest
{
    // TODO: disabled until non-copyable types work in AA's
    // string[Z] aa;
    // aa[123.Z] = "abc";
    // aa[456.Z] = "def";
    // assert(aa[123.Z] == "abc");
    // assert(aa[456.Z] == "def");
}

/// copyable integer
@trusted unittest
{
    alias CZ = CopyableMpZ;

    CZ a = 42;

    CZ b = a;                   // copy
    assert(a == b);             // should equal
    assert(a !is b);            // but not the same

    CZ c = null;                // other value
    assert(a != c);             // should diff
}

/// to string conversion
pure @safe nothrow unittest
{
    for (int i = -100; i < 100; ++i)
    {
        import std.conv : to;
        assert(i.Z.toString == i.to!string);
    }
}

version(unittest)
{
    // version = ccc;              // do C mutation call count
    debug import core.stdc.stdio : printf;
    static assert(!isMpZExpr!int);
    import std.meta : AliasSeq;
	alias Z = MpZ;
    alias CZ = CopyableMpZ;
    alias RZ = _Z!(true);
}

// C API
extern(C) pragma(inline, false)
{
    alias __mp_limb_t = ulong;    // see `mp_limb_t` gmp.h. TODO: detect when it is `uint` instead
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
    void *__gmpz_export (void* , size_t*, int, size_t, int, size_t, mpz_srcptr);
    void __gmpz_import (mpz_ptr, size_t, int, size_t, int, size_t, const void *);

    void __gmpz_powm (mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr);
    void __gmpz_powm_ui (mpz_ptr, mpz_srcptr, ulong, mpz_srcptr);

    int __gmpz_invert (mpz_ptr, mpz_srcptr, mpz_srcptr);

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

    void __gmpz_setbit (mpz_ptr, mp_bitcnt_t);
    void __gmpz_clrbit (mpz_ptr, mp_bitcnt_t);
    void __gmpz_combit (mpz_ptr, mp_bitcnt_t);
    int __gmpz_tstbit (mpz_srcptr, mp_bitcnt_t);

    mp_bitcnt_t __gmpz_popcount (mpz_srcptr);

    int  __gmpz_root (mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_rootrem (mpz_ptr, mpz_ptr, mpz_srcptr, ulong);
    void __gmpz_sqrt (mpz_ptr, mpz_srcptr);
    void __gmpz_sqrtrem (mpz_ptr, mpz_ptr, mpz_srcptr);	// TODO: wrap
    int __gmpz_perfect_power_p (mpz_srcptr);
    int __gmpz_perfect_square_p (mpz_srcptr);
}

pragma(lib, "gmp");
pragma(lib, "c");               // needed by `malloc` and `free`
