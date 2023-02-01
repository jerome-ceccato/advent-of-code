#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"
#include "utils.h"

int day6_decode_c(char c) {
    if (c >= 'A' && c <= 'Z')
        return c - 'A';
    return 26 + c - '0';
}

int* day6_map_orbits(const char* input) {
    // At most 3 characters [A-Z0-9], can be uniquely encoded into a 0-36^3 int
    // Technically we should +1 everything to keep 0 as the 'empty' value,
    // but in practice there are no orbits called "A" in the input.
    int* map = calloc(sizeof(*map), (36 * 36 * 36));

    int i = 0;
    while (input[i]) {
        int lhs = 0, rhs = 0;

        for (; input[i] != ')'; i++)
            lhs = lhs * 36 + day6_decode_c(input[i]);
        for (++i; input[i] != '\n' && input[i] != '\0'; i++)
            rhs = rhs * 36 + day6_decode_c(input[i]);
        map[rhs] = lhs;

        if (input[i])
            i++;
    }
    return map;
}

int day6_count_orbits(int* map, int i) {
    int total = 0;
    while (map[i]) {
        i = map[i];
        total++;
    }
    return total;
}

char* day6p1(const char* input) {
    int* map = day6_map_orbits(input);

    int total = 0;
    for (int i = 0; i < (36 * 36 * 36); i++) {
        if (map[i]) {
            total += day6_count_orbits(map, i);
        }
    }

    free(map);
    return aoc_asprintf("%d", total);
}

char* day6p2(const char* input) {
    int* map = day6_map_orbits(input);

    int you = (day6_decode_c('Y') * 36 * 36) + (day6_decode_c('O') * 36) + day6_decode_c('U');
    int san = (day6_decode_c('S') * 36 * 36) + (day6_decode_c('A') * 36) + day6_decode_c('N');

    int min_orbits = INT_MAX;
    int san_p = map[san];
    // Go backwards from each object until a common ancestor is found
    for (int san_i = 0; map[san_p]; san_i++, san_p = map[san_p]) {
        int you_p = map[you];
        for (int you_i = 0; map[you_p]; you_i++, you_p = map[you_p]) {
            if (san_p == you_p) {
                min_orbits = min(min_orbits, san_i + you_i);
            }
        }
    }

    free(map);
    return aoc_asprintf("%d", min_orbits);
}
