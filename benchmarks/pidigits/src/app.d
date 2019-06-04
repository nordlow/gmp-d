import gmp;

@safe:

alias Z = MpZ;

class LFT
{
@safe pure nothrow @nogc:

    public:
    Z q;
    Z r;
    Z t;
    uint k;

    Z tmp0;
    Z tmp1;
    
    // Replace static init of data members
    void init()
    {
        q = 1;
        r = 0;
        t = 1;
        k = 0;
    }

    void next()
    {
        ++k;
        r = (2 * k + 1) * (2 * q + r);
        t = (2 * k + 1) * t;
        q = q * k;
    }

    uint extract(uint x)        // TODO `const` 
    {
        tmp0 = q * x + r;
        tmp1 = tmp0 / t;
        return cast(uint)tmp1;
    }

    void produce(uint n)
    {
        q = 10 * q;
        r = 10 * (r - n * t);
    }
};

int main(string[] args)
{
    import std.stdio:  write, writeln;
    import std.conv : to;
    
    if (args.length <= 1)
    {
        writeln("Usage: N");
        return 1;
    }
    const size_t total_digits = args[1].to!size_t;

    LFT lft;
    size_t n_digits = 0;
    while (n_digits < total_digits)
    {
        size_t i = 0;
        while (i < 10 && n_digits < total_digits)
        {
            lft.next();
            if (lft.q > lft.r) continue;

            auto digit = lft.extract(3);
            if (digit == lft.extract(4))
            {
                write(digit);
                lft.produce(digit);
                ++i;
                ++n_digits;
            }
        }

        // Pad digits with extra spaces if total_digits was not a
        // multiple of 10.
        for (; i < 10; ++i) write(' ');
        write("\t:", n_digits, '\n');
    }

    return 0;
}
