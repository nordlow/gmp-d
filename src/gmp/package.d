/** High-level wrapper for GNU Multiple Precision (MP) library.
*/
module gmp;

public import gmp.z;
public import gmp.q;
public import gmp.f;

pragma(lib, "gmp");
