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

    return (a == b) ? 5 : 0;
}