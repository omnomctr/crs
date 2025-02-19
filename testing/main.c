int main(void)
{
    int a = 4;
    a <<= 1;
    a <<= 1;
    a >>= 1;
    a >>= 1;
    int b = 32590;
    b++;
    --b;

    if (b > 10) {
        b ^= b;
    } else {
        return a;
    }

    int x = 0;
    while (x++ < 100) {
        int y = 0;
        while (y++ < 100)
            ;
    }

    int c = 4;
    {
        int c = 5;
    }


    return (x == b) ? 5 : c;
}