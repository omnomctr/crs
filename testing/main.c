int put_int(int n);

int fib(int n)
{
    if (n == 1)
        return 1;
    else if (n <= 0)
        return 0;
    else
        return fib(n - 1) + fib(n - 2);
}

int main()
{
    for (int i = 1; i <= 15; i++)
        put_int(fib(i));

    return 0;
}