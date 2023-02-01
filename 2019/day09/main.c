#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"

char* day9p1(const char* input) {
    t_intcode_result result = aoc_intcode_eval(input, intcode_seed_input1(1));
    bigint out = result.state.output.data[result.state.output.head - 1];
    intcode_free_result(&result);
    return aoc_bigint_to_str(out);
}

char* day9p2(const char* input) {
    t_intcode_result result = aoc_intcode_eval(input, intcode_seed_input1(2));
    bigint out = result.state.output.data[result.state.output.head - 1];
    intcode_free_result(&result);
    return aoc_bigint_to_str(out);
}
