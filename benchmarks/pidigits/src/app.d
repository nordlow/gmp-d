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

void main(string[] args)
{
    // const std::size_t TOTAL_DIGITS = std::atol(argv[1]);

    // LFT lft;
    // std::size_t n_digits = 0;
    // while (n_digits < TOTAL_DIGITS) {
    //     std::size_t i = 0;
    //     while (i < 10 and n_digits < TOTAL_DIGITS) {
    //         lft.next();
    //         if (lft.q > lft.r) continue;

    //         auto digit = lft.extract(3);
    //         if (digit == lft.extract(4)) {
    //             std::cout << digit;
    //             lft.produce(digit);
    //             ++i;
    //             ++n_digits;
    //         }
    //     }

    //     // Pad digits with extra spaces if TOTAL_DIGITS was not a
    //     // multiple of 10.
    //     for (; i < 10; ++i) std::cout << ' ';
    //     std::cout << "\t:" << n_digits << '\n';
    // }
}
