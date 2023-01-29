#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"
#include "utils.h"

void day2_setup_memory(int* memory, size_t __attribute__((unused)) size) {
    memory[1] = 12;
    memory[2] = 2;
}

char* day2p1(const char* input) {
    t_intcode_result result = aoc_intcode_eval(input, day2_setup_memory);

    int res = result.memory[0];

    intcode_free_result(&result);
    return aoc_asprintf("%d", res);
}

char* day2p2(const char* input) {
    return NULL;
}
