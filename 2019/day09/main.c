#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"

static t_intcode_result run_intcode(const char* input, bigint mode) {
    t_intcode_state state = aoc_intcode_boot(input);
    aoc_intcode_upgrade_memory(&state, 2048);
    intcode_set_input1(&state, mode);

    return aoc_intcode_eval(state);
}

char* day9p1(const char* input) {
    t_intcode_result result = run_intcode(input, 1);
    bigint out = intcode_last_output(&result);
    intcode_free_result(&result);
    return aoc_bigint_to_str(out);
}

char* day9p2(const char* input) {
    t_intcode_result result = run_intcode(input, 2);
    bigint out = intcode_last_output(&result);
    intcode_free_result(&result);
    return aoc_bigint_to_str(out);
}
