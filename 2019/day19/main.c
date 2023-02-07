#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

static t_intcode_result run_intcode(const char* input, int x, int y) {
    t_intcode_state state = aoc_intcode_boot(input);
    aoc_intcode_upgrade_memory(&state, 1024);
    intcode_set_input2(&state, x, y);

    return aoc_intcode_eval(state);
}

static bool pulled_at(const char* input, int x, int y) {
    t_intcode_result result = run_intcode(input, x, y);
    bool pulled = intcode_last_output(&result);
    intcode_free_result(&result);
    return pulled;
}

static bool has_square(const char* input, int size, int ogx, int ogy) {
    // only checking the borders is needed
    for (int i = 0; i < size; i++) {
        if (!pulled_at(input, ogx + i, ogy)
            || !pulled_at(input, ogx + (size - 1) - i, ogy)
            || !pulled_at(input, ogx, ogy + i)
            || !pulled_at(input, ogx, ogy + (size - 1) - i)) {
            return false;
        }
    }
    return true;
}

static t_point2d find_square(const char* input, int size) {
    t_point2d origin = point2d_make(0, 10);

    for (;; origin.y++) {
        int row_size = 0;
        for (; !pulled_at(input, origin.x, origin.y); origin.x++) {}
        for (; pulled_at(input, origin.x + row_size, origin.y); row_size++) {}

        for (int x = 0; (row_size - x) >= size; x++) {
            if (has_square(input, size, origin.x + x, origin.y)) {
                return point2d_make(origin.x + x, origin.y);
            }
        }
    }
}

char* day19p1(const char* input) {
    int pulled = 0;
    for (int y = 0; y < 50; y++)
        for (int x = 0; x < 50; x++)
            pulled += pulled_at(input, x, y);

    return aoc_asprintf("%d", pulled);
}

char* day19p2(const char* input) {
    t_point2d square = find_square(input, 100);

    return aoc_asprintf("%d", square.x * 10000 + square.y);
}
