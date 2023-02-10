#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

#ifdef __GNUC__
typedef __int128_t hugeint;
#else
typedef bigint hugeint;
#endif

typedef struct
{
    hugeint a;
    hugeint b;
} t_linear_eq;

// I totally figured that out by myself and didn't lookup hints on reddit
static t_linear_eq compose(t_linear_eq lhs, t_linear_eq rhs, hugeint deck_size) {
    return (t_linear_eq){.a = (lhs.a * rhs.a) % deck_size, (lhs.b * rhs.a + rhs.b) % deck_size};
}

static hugeint resolve(t_linear_eq eq, hugeint x, hugeint deck_size) {
    return (((eq.a * x + eq.b) % deck_size) + deck_size) % deck_size;
}

static bool has_prefix(const char *str, const char *prefix) {
    size_t str_sz = strlen(str);
    size_t prefix_sz = strlen(prefix);

    if (str_sz >= prefix_sz)
        return strncmp(prefix, str, prefix_sz) == 0;
    return false;
}

static t_linear_eq compile_shuffle_techniques(const char *input, hugeint deck_size) {
    t_linear_eq acc = (t_linear_eq){.a = 1, .b = 0};
    while (input) {
        if (has_prefix(input, "deal into new stack")) {
            acc = compose(acc, (t_linear_eq){.a = -1, .b = -1}, deck_size);
        } else if (has_prefix(input, "cut ")) {
            int n = strtol(input + strlen("cut "), NULL, 10);
            acc = compose(acc, (t_linear_eq){.a = 1, .b = -n}, deck_size);

        } else if (has_prefix(input, "deal with increment ")) {
            int n = strtol(input + strlen("deal with increment "), NULL, 10);
            acc = compose(acc, (t_linear_eq){.a = n, .b = 0}, deck_size);
        } else {
            fprintf(stderr, "Unrecognized action %s\n", input);
        }

        const char *p = strchr(input, '\n');
        input = p ? p + 1 : NULL;
    }
    return acc;
}

static t_linear_eq compose_exp(t_linear_eq eq, hugeint times, hugeint deck_size) {
    t_linear_eq next = (t_linear_eq){.a = 1, .b = 0};
    while (times > 0) {
        if (times & 1)
            next = compose(next, eq, deck_size);
        times /= 2;
        eq = compose(eq, eq, deck_size);
    }
    return next;
}

// Taken from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
static hugeint mod_inverse(hugeint a, hugeint n) {
    hugeint t = 0, r = n, newt = 1, newr = a;

    while (newr != 0) {
        hugeint q = r / newr;
        hugeint tmp = t;
        t = newt;
        newt = tmp - q * newt;
        tmp = r;
        r = newr;
        newr = tmp - q * newr;
    }

    if (t < 0)
        t += n;
    return t;
}

char *day22p1(const char *input) {
    const int deck_size = 10007;
    t_linear_eq eq = compile_shuffle_techniques(input, deck_size);
    return aoc_asprintf("%d", (int)resolve(eq, 2019, deck_size));
}

char *day22p2(const char *input) {
    const hugeint deck_size = 119315717514047ll;
    const hugeint steps = 101741582076661ll;

    t_linear_eq eq = compile_shuffle_techniques(input, deck_size);
    eq = compose_exp(eq, steps, deck_size);

    hugeint result = (((hugeint)2020 - eq.b) * mod_inverse(eq.a, deck_size)) % deck_size + deck_size;
    return aoc_bigint_to_str((bigint)result);
}
