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
label:
    x++;
    if (x < 100)
        goto label;

    return (a == b) ? 5 : x;
}