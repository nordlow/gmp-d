/// Multiple precision rational numbers (Q).
module gmp.q;

import gmp.traits;
import gmp.z;

@safe:

/** Arbitrary (multi) precision rational number (Q).
 *
 * Wrapper for GNU MP (GMP)'s type `mpq_t` and functions `__gmpq_.*`.
 */
struct MpQ
{
	pure nothrow:

	/// Convert to `string` in base `base`.
	string toString(in uint base = defaultBase,
					in bool upperCaseDigits = false) const
	{
		assert((base >= -2 && base <= -36) ||
			   (base >= 2 && base <= 62));
		// TODO: use on allocation only
		return (numerator.toString(base, upperCaseDigits) ~
				"/" ~
				denominator.toString(base, upperCaseDigits));
	}

	/** Convert in base `base` into `chars` of length `length`.
	 *
	 * Returns: char[] which must be freed manually with `pureFree`.
	 */
	char[] toChars(in uint base = defaultBase,
				   in bool upperCaseDigits = false) const @system @nogc
	{
		assert((base >= -2 && base <= -36) ||
			   (base >= 2 && base <= 62));
		assert(false, "TODO: use on allocation only");
	}

	pragma(inline, true):

	// TODO: toRCString wrapped in UniqueRange

	@nogc:

	/** No default construction for now, because `mpq_init` initialize
	 * `__mpq_struct`-fields to non-zero values.
	 *
	 * TODO: Allow default construction by delaying call to initialize().
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
	this(P)(in P value) @safe
	if (__traits(isFloating, P))
	{
		initialize();
		this = value;		   // reuse opAssign
	}

	/** Construct from `pValue` / `qValue`.
	 *
	 * Note that `qValue` must be explicitly given, to prevent accidental
	 * storage of integers as rations with denominator being 1.
	 */
	this(P, Q)(P pValue, Q qValue,
			   in bool canonicalizeFlag = false) @trusted
	if (__traits(isIntegral, P) &&
		__traits(isIntegral, Q))
	{
		initialize();

		static if (isSigned!Q)
			assert(qValue >= 1, "Negative denominator");

		// dln("qValue:", qValue);

		static	  if (isUnsigned!P)
			__gmpq_set_ui(_ptr, pValue, qValue);
		else					// signed integral
			__gmpq_set_si(_ptr, pValue, qValue);

		if (canonicalizeFlag)
			canonicalize();
	}

	/** Construct from floating-point `value`.
	 */
	ref MpQ opAssign(P)(P value) @trusted scope return
	if (__traits(isFloating, P))
	{
		version(DigitalMars) pragma(inline, false);
		__gmpq_set_d(_ptr, value);
		return this;
	}

	/** Assign from integer `value`. */
	ref MpQ opAssign(P)(P value) @trusted scope return
	if (__traits(isIntegral, P))
	{
		version(DigitalMars) pragma(inline, false);

		static	  if (isUnsigned!P)
			__gmpq_set_ui(_ptr, value, 1);
		else					// signed integral
			__gmpq_set_si(_ptr, value, 1);

		return this;
	}

	/** Canonicalize `this`. */
	void canonicalize() @trusted
	{
		__gmpq_canonicalize(_ptr);
	}

	/// Destruct `this`.
	~this() @trusted @nogc
	{
		assert(_ptr, "Pointer is null");
		__gmpq_clear(_ptr);
	}

	/// Returns: `true` iff `this` equals `rhs`.
	bool opEquals()(auto ref const MpQ rhs) const @trusted
	{
		version(DigitalMars) pragma(inline, false);
		if (_ptr == rhs._ptr)   // fast equality
			return true;		// fast bailout
		return __gmpq_equal(_ptr, rhs._ptr) != 0;
	}
	/// ditto
	int opEquals(T)(T rhs) const @safe
	if (__traits(isIntegral, T))
	{
		if (rhs == 0)
			return numerator.isZero; // optimization
		return numerator == rhs && denominator == 1;
	}

	/// Compare `this` to `rhs`.
	int opCmp()(auto ref const MpQ rhs) const @trusted
	{
		version(DigitalMars) pragma(inline, false);
		if (rhs.numerator == 0)
			return sgn;		 // optimization
		return __gmpq_cmp(_ptr, rhs._ptr);
	}
	/// Compare `this` to `rhs`.
	int opCmp()(auto ref const MpZ rhs) const @trusted
	{
		version(DigitalMars) pragma(inline, false);
		if (rhs == 0)
			return sgn;		 // optimization
		return __gmpq_cmp_z(_ptr,
							cast(const(__mpz_struct)*)&rhs); // TODO: wrap cast?
	}
	/// ditto
	int opCmp(T)(T rhs) const @trusted
	if (__traits(isIntegral, T))
	{
		if (rhs == 0)
			return sgn;		 // optimization
		static if (isUnsigned!T)
			return __gmpq_cmp_ui(_ptr, rhs, 1UL);
		else					// isSigned integral
			return __gmpq_cmp_si(_ptr, rhs, 1UL);
	}

	/// Get the hash suitable for use in a hash table.
	size_t toHash() const @safe
	{
		return (numerator.toHash ^
				denominator.toHash);
	}

	/// Returns: numerator reference of `this`.
	@property ref inout(MpZ) numerator() @trusted inout scope return
	{
		return *(cast(inout(MpZ)*)_num_ptr);
	}

	/// Returns: denominator reference of `this`.
	@property ref inout(MpZ) denominator() @trusted inout scope return
	{
		return *(cast(inout(MpZ)*)_den_ptr);
	}

	/// Returns: the integer part of `this`, with any remainder truncated.
	@property MpZ integerPart() @safe
	{
		return numerator / denominator;
	}

	/// Returns: the fractional part of `this`.
	// TODO: activate when sub(MpQ, MpZ) has been added
	// @property MpQ fractionPart()
	// {
	//	 return this - integerPart;
	// }

	/// Cast to arithmetic type `T`.
	T opCast(T)() const @trusted /*TODO: scope*/
	if (__traits(isFloating, T))
	{
		return cast(T)__gmpq_get_d(_ptr);
	}

	/** Invert `this` in-place.
	 *
	 * Returns: `void` to make it obvious that `this` is mutated.
	 */
	void invert() @trusted
	{
		version(DigitalMars) pragma(inline, false);
		import std.algorithm.mutation : swap;
		const bool negative = numerator < 0;
		if (negative)
		{
			numerator.absolute();		 // fast inline
			swap(numerator, denominator); // fast inline
			numerator.negate();		   // fast inline
		}
		else
			swap(numerator, denominator); // fast inline
	}

	/** Returns: sign as either
		- -1 (`this` < 0),
		-  0 (`this` == 0), or
		- +1 (`this` > 0).
	*/
	@property int sgn() const @safe
	{
		assert(denominator >= 1);
		return numerator.sgn;   // sign always stored in numerator so reuse fast
	}

	/** Make `this` the absolute value of itself in-place.
	 *
	 * Returns: `void` to make it obvious that `this` is mutated.
	 */
	void absolute() @safe
	{
		numerator.absolute();
	}

	MpQ opBinary(string s)(auto ref const MpQ rhs) const @trusted // direct value
		if ((s == "+" || s == "-" ||
			 s == "*" || s == "/"))
	{
		version(DigitalMars) pragma(inline, false);
		static if (!__traits(isRef, rhs)) // r-value `rhs`
		{
			MpQ* mut_rhs = (cast(MpQ*)(&rhs)); // @trusted because `MpQ` has no aliased indirections
			static	  if (s == "+")
				__gmpq_add(mut_rhs._ptr, _ptr, rhs._ptr);
			else static if (s == "-")
				__gmpq_sub(mut_rhs._ptr, _ptr, rhs._ptr);
			else static if (s == "*")
				__gmpq_mul(mut_rhs._ptr, _ptr, rhs._ptr);
			else static if (s == "/")
			{
				assert(rhs != 0, "Divison by zero");
				__gmpq_div(mut_rhs._ptr, _ptr, rhs._ptr);
			}
			else
				static assert(false);
			import core.lifetime : move;
			return move(*mut_rhs); // TODO: shouldn't have to call `move` here
		}
		else
		{
			typeof(return) y = null;
			static	  if (s == "+")
				__gmpq_add(y._ptr, _ptr, rhs._ptr);
			else static if (s == "-")
				__gmpq_sub(y._ptr, _ptr, rhs._ptr);
			else static if (s == "*")
				__gmpq_mul(y._ptr, _ptr, rhs._ptr);
			else static if (s == "/")
			{
				assert(rhs != 0, "Divison by zero");
				__gmpq_div(y._ptr, _ptr, rhs._ptr);
			}
			else
				static assert(false);
			return y;
		}
	}

	// /// Divided integral with `this`.
	// Unqual!Lhs opBinaryRight(string s, Lhs)(Lhs lhs) const @trusted
	//	 if ((s == "/") &&
	//		 __traits(isIntegral, Lhs))
	// {
	//	 if (lhs == 1)
	//	 {
	//		 return inv(this);
	//	 }
	//	 else
	//	 {
	//		 MpQ y = null; // TODO: avoid if !__traits(isRef, this)
	//		 assert(this != 0, "Divison by zero");
	//		 denominator *= lhs;
	//		 __gmpq_div(y._ptr, MpZ(lhs)._ptr, _ptr);
	//		 return y;
	//	 }
	// }

private:

	/// Default conversion base.
	enum defaultBase = 10;

	/** Initialize internal struct. */
	private void initialize() @trusted // cannot be called `init` as that will override builtin type property
	{
		__gmpq_init(_ptr);
	}

	/// Returns: pointer to internal rational C struct.
	inout(__mpq_struct)* _ptr() inout return @system
	{
		return &_q;
	}

	/// Returns: pointer to internal numerator C struct.
	inout(__mpz_struct)* _num_ptr() inout return @system
	{
		return cast(typeof(return))&_q._mp_num;
	}

	/// Returns: pointer to internal denominator C struct.
	inout(__mpz_struct)* _den_ptr() inout return @system
	{
		return cast(typeof(return))&_q._mp_den;
	}

	__mpq_struct _q;			// internal libgmp C struct

	version(ccc)
	{
		/** Number of calls made to `__gmpq`--functions that construct or
		 * changes this value. Used to verify correct lowering and evaluation of
		 * template expressions.
		 *
		 * For instance the `x` in `x = y + z` should be assigned only once
		 * inside a call to `mpq_add`.
		 */
		@property size_t mutatingCallCount() const @safe { return _ccc; }

		/** C mutation call count. Number of calls to C GMP function calls that
		 * mutate this object.
		 */
		size_t _ccc;
	}
}

pure nothrow pragma(inline, true):

/// Swap contents of `x` with contents of `y`.
void swap()(ref MpQ x,
			ref MpQ y)
{
	import std.algorithm.mutation : swap;
	swap(x, y); // x.swap(y);
}

/// Returns: absolute value of `x`.
MpQ abs()(auto ref const MpQ x) @trusted
{
	version(DigitalMars) pragma(inline, false);
	static if (__traits(isRef, x)) // l-value `x`
	{
		MpQ y = null;
		__gmpq_abs(y._ptr, x._ptr);
		return y;
	}
	else						// r-value `x`
	{
		MpQ* mut_x = (cast(MpQ*)(&x)); // @trusted because `MpQ` has no aliased indirections
		mut_x.absolute();
		import core.lifetime : move;
		return move(*mut_x);	// TODO: shouldn't have to call `move` here
	}
}

/// Returns: inverse of `x`.
MpQ inverse()(auto ref const MpQ x) @trusted
{
	version(DigitalMars) pragma(inline, false);
	static if (__traits(isRef, x)) // l-value `x`
	{
		MpQ y = null;
		__gmpq_inv(y._ptr, x._ptr);
		return y;
	}
	else						// r-value `x`
	{
		MpQ* mut_x = (cast(MpQ*)(&x)); // @trusted because `MpQ` has no aliased indirections
		mut_x.invert();
		import core.lifetime : move;
		return move(*mut_x);	// TODO: shouldn't have to call `move` here
	}
}
alias inv = inverse;

/// construction and assignment
@safe @nogc version(gmp_test) unittest
{
	Q x = null;
	assert(x.numerator == 0);
	assert(x.denominator == 1);

	const Q y = Q(11, 13UL);
	assert(y.numerator == 11);
	assert(y.denominator == 13);

	const Q z = Q(7UL, 13UL);
	assert(z.numerator == 7);
	assert(z.denominator == 13);

	Q w = 0.25;				 // construct from `double`
	assert(w.numerator == 1);
	assert(w.denominator == 4);

	w = 0.125f;				 // assign from `float`
	assert(w.numerator == 1);
	assert(w.denominator == 8);

	w = 2;					  // assign from `int`
	assert(w.numerator == 2);
	assert(w.denominator == 1);

	w = 3;					  // assign from `int`
	assert(w.numerator == 3);
	assert(w.denominator == 1);
}

/// canonicalization
@safe @nogc version(gmp_test) unittest
{
	Q x = Q(2, 4);
	assert(x.numerator == 2);
	assert(x.denominator == 4);
	x.canonicalize();
	assert(x.numerator == 1);
	assert(x.denominator == 2);
}

/// negative numerator canonicalization
@safe @nogc version(gmp_test) unittest
{
	Q x = Q(-2, 4);
	assert(x.numerator == -2);
	assert(x.denominator == 4);
	x.canonicalize();
	assert(x.numerator == -1);
	assert(x.denominator == 2);
}

/// swap
@safe @nogc version(gmp_test) unittest
{
	Q x = Q(1, 2);
	Q y = Q(1, 3);
	swap(x, y);
	assert(x == Q(1, 3));
	assert(y == Q(1, 2));
}

/// invert
@safe version(gmp_test) unittest
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

/// inversion
@safe version(gmp_test) unittest
{
	const Q q = Q(-2, 3);
	assert(inverse(q) == Q(-3, 2));

	assert(inverse(Q(2, 3)) == Q(3, 2));
	assert(inverse(Q(1, 10)) == 10);
	assert(inverse(Q(10, 1)) == Q(1, 10));
}

/// absolute value
@safe version(gmp_test) unittest
{
	const Q q = Q(-2, 3);
	assert(abs(q) == Q(2, 3));
	assert(abs(Q(-2, 3)) == Q(2, 3));
}

/// integer and fractional part
@safe version(gmp_test) unittest
{
	Q x = Q(5, 2);

	assert(x.integerPart == 2);
	// TODO: assert(x.fractionalPart == Q(1, 2));

	x = Q(7, 2);
	assert(x.integerPart == 3);
	// TODO: assert(x.fractionalPart == Q(1, 2));

	x = Q(10, 2);
	assert(x.integerPart == 5);
	// TODO: assert(x.fractionalPart == 0);

	x = Q(11, 3);
	assert(x.integerPart == 3);
	// TODO: assert(x.fractionalPart == Q(2, 3));

	x = Q(12, 2);
	assert(x.integerPart == 6);
	// TODO: assert(x.fractionalPart == 0);
}

/// casting
@safe @nogc version(gmp_test) unittest
{
	assert(cast(double)Q(1, 2) == 0.5f);
	assert(cast(double)Q(1, 2) == 0.5);
	assert(cast(double)Q(2, 4) == 0.5);
	assert(cast(double)Q(1, 8) == 1.0/8);
}

/// equality
@safe version(gmp_test) unittest
{
	assert(Q(1, 1) == 1);
	assert(Q(2, 1) == 2);
	assert(Q(1, 1) == Q(1, 1));
	assert(Q(1, 1) != Q(1, 2));
	const x = Q(1, 3);
	assert(x == x);			 // same
}

/// sign
@safe version(gmp_test) unittest
{
	assert(Q(-1, 3).sgn == -1);
	assert(Q( 0, 3).sgn ==  0);
	assert(Q( 1, 3).sgn ==  1);
}

/// comparison
@safe version(gmp_test) unittest
{
	assert(Q( 1, 3) < Q(1, 2));
	assert(Q( 1, 2) > Q(1, 3));
	assert(Q( 1, 2) > Q(0, 1));
	assert(Q( 0, 1) == Q(0, 1));
	assert(Q( 0, 2) == Q(0, 1));
	assert(Q(-1, 2) < Q(0, 1));

	assert(Q( 1, 3) < 1);
	assert(Q( 1, 3) > 0);
	assert(Q(-1, 3) < 0);

	assert(Q( 1, 3) < 1UL);
	assert(Q( 1, 3) > 0UL);
	assert(Q(-1, 3) < 0UL);

	assert(Q( 1, 3) < 1.Z);
	assert(Q( 1, 3) > 0.Z);
	assert(Q(-1, 3) < 0.Z);
}

/// addition
@safe version(gmp_test) unittest
{
	assert(Q(1, 2) + Q(1, 2) == Q(1, 1));
	assert(Q(1, 3) + Q(1, 3) == Q(2, 3));
	assert(Q(1, 2) + Q(1, 3) == Q(5, 6));
}

/// subtraction
@safe version(gmp_test) unittest
{
	assert(Q(1, 2) - Q(1, 2) == Q( 0, 1));
	assert(Q(1, 2) - Q(1, 3) == Q (1, 6));
	assert(Q(1, 3) - Q(1, 2) == Q(-1, 6));
}

/// multiplication
@safe version(gmp_test) unittest
{
	assert(Q(1, 2) * Q(1, 2) == Q(1, 4));
	assert(Q(2, 3) * Q(2, 3) == Q(4, 9));
	assert(Q(1, 2) * Q(1, 3) == Q(1, 6));
}

/// division
@safe version(gmp_test) unittest
{
	assert(Q(2, 3) / Q(2, 3) == Q(1, 1));
	assert(Q(2, 3) / Q(2, 3) == 1);
	assert(Q(2, 3) / Q(3, 2) == Q(4, 9));
	assert(Q(3, 2) / Q(2, 3) == Q(9, 4));
	// TODO: assert(1 / Q(2, 3) == Q(3, 2));
}

version(gmp_test) version(unittest)
{
	// version = ccc;			  // do C mutation call count
	alias Z = MpZ;
	alias Q = MpQ;
	static assert(!isMpZExpr!int);
	import std.meta : AliasSeq;
}

// C API
package extern(C) pragma(inline, false)
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

	int __gmpq_equal (mpq_srcptr, mpq_srcptr);
	int __gmpq_cmp (mpq_srcptr, mpq_srcptr);
	int __gmpq_cmp_z (mpq_srcptr, mpz_srcptr);

	int __gmpq_cmp_ui (mpq_srcptr, ulong, ulong);
	int __gmpq_cmp_si (mpq_srcptr, long, ulong);

	void __gmpq_add (mpq_ptr, mpq_srcptr, mpq_srcptr);
	void __gmpq_sub (mpq_ptr, mpq_srcptr, mpq_srcptr);
	void __gmpq_mul (mpq_ptr, mpq_srcptr, mpq_srcptr);
	void __gmpq_div (mpq_ptr, mpq_srcptr, mpq_srcptr);

	void __gmpq_abs (mpq_ptr, mpq_srcptr);
	void __gmpq_inv (mpq_ptr, mpq_srcptr);
}

pragma(lib, "gmp");
