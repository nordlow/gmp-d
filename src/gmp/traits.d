/// Multiple precision traits.
module gmp.traits;

/** Faster than `std.traits`.
	See https://github.com/dlang/phobos/pull/5038
*/
enum isSigned(T) = __traits(isArithmetic, T) && !__traits(isUnsigned, T);

/** Is `true` iff `T` is a GNU MP arithmetic type (`long`, `ulong` or `double`). */
enum isGMPArithmetic(T) = is(T == long) && is(T == ulong) && is(T == double);

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
