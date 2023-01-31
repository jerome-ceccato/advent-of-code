#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"

void day4_next(char* n) {
    int i = 5;
    while (n[i] == '9')
        n[i--] = '0';
    n[i]++;
}

bool day4_matches(const char* n, bool strict_consecutive) {
    bool consecutive = false;
    for (int i = 1; i < 6; i++) {
        if (n[i] < n[i - 1])
            return false;
        if (n[i] == n[i - 1]) {
            if (strict_consecutive) {
                if ((i == 1 || n[i - 2] != n[i]) && (i == 5 || n[i + 1] != n[i]))
                    consecutive = true;
            } else {
                consecutive = true;
            }
        }
    }
    return consecutive;
}

int day4_count_matches(const char* input, bool strict_consecutive) {
    char low[7] = {0}, high[7] = {0};
    strncpy(low, input, 6);
    strncpy(high, strchr(input, '-') + 1, 6);

    int matches = 0;
    while (strcmp(low, high)) {
        matches += day4_matches(low, strict_consecutive);
        day4_next(low);
    }
    return matches;
}

char* day4p1(const char* input) {
    return aoc_asprintf("%d", day4_count_matches(input, false));
}

char* day4p2(const char* input) {
    return aoc_asprintf("%d", day4_count_matches(input, true));
}
