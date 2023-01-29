#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"

char* day1p1(const char* input) {
    size_t size = 0;
    int* numbers = NULL;

    aoc_contents_to_ints(input, '\n', &numbers, &size);

    int total = 0;
    for (size_t i = 0; i < size; i++) {
        total += numbers[i] / 3 - 2;
    }
    return aoc_asprintf("%d", total);
}

char* day1p2(const char* input) {
    size_t size = 0;
    int* numbers = NULL;

    aoc_contents_to_ints(input, '\n', &numbers, &size);

    int total = 0;
    for (size_t i = 0; i < size; i++) {
        int n = numbers[i];
        while (n > 0) {
            n = n / 3 - 2;
            if (n > 0)
                total += n;
        }
    }
    return aoc_asprintf("%d", total);
}
