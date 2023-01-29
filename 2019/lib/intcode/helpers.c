#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "intcode.h"
#include "aoc.h"

bool intcode_safe_read(int* memory, size_t size, size_t i, int* out) {
    if (i < size) {
        *out = memory[i];
        return true;
    }
    fprintf(stderr, "intcode attempting to read oob %zu\n", i);
    return false;
}

bool intcode_safe_write(int* memory, size_t size, size_t i, int val) {
    if (i < size) {
        memory[i] = val;
        return true;
    }
    fprintf(stderr, "intcode attempting to write oob %zu\n", i);
    return false;
}

void intcode_free_result(t_intcode_result* res) {
    if (res) {
        free(res->memory);
    }
}
