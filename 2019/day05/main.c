#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"

static t_intcode_result run_intcode(const char* input, bigint program_input) {
    t_intcode_state state = aoc_intcode_boot(input);
    intcode_set_input1(&state, program_input);

    return aoc_intcode_eval(state);
}

char* day5p1(const char* input) {
    t_intcode_result result = run_intcode(input, 1);
    bigint res = intcode_last_output(&result);

    intcode_free_result(&result);
    return aoc_bigint_to_str(res);
}

char* day5p2(const char* input) {
    t_intcode_result result = run_intcode(input, 5);
    bigint res = intcode_last_output(&result);

    intcode_free_result(&result);
    return aoc_bigint_to_str(res);
}
