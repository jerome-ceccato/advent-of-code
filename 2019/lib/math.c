#include <stdlib.h>
#include "aoc.h"

bigint aoc_gcd(bigint a, bigint b) {
    bigint tmp;
    while (b != 0) {
        tmp = a % b;
        a = b;
        b = tmp;
    }
    return a;
}

bigint aoc_lcm(bigint a, bigint b) {
    return (a * b) / aoc_gcd(a, b);
}
