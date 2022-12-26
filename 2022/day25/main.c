#include <stdio.h>
#include <string.h>

typedef unsigned long long bignum_t;

bignum_t decode(int c)
{
    static const char *const digits = "=-012";
    return (bignum_t)(strchr(digits, c) - digits) - 2;
}

void print_snafu(bignum_t n)
{
    char buffer[128] = {0};
    int i = sizeof(buffer) - 2;

    while (n > 0)
    {
        bignum_t rem = n % 5;
        buffer[i--] = "012=-"[rem];
        n = n / 5 + (rem > 2);
    }

    puts(buffer + i + 1);
}

int main(void)
{

    FILE *fd = fopen("input", "r");
    int c = 0;
    bignum_t sum = 0, n = 0;

    while ((c = fgetc(fd)) != EOF)
    {
        if (c == '\n')
        {
            sum += n;
            n = 0;
        }
        else
        {
            n = n * 5 + decode(c);
        }
    }

    sum += n;
    print_snafu(sum);

    fclose(fd);
    return 0;
}
