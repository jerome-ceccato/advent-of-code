#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"

void day5p1_setup_input(t_intcode_state* state) {
    state->input.data = malloc(sizeof(*state->input.data));
    state->input.size = 1;
    state->input.data[0] = 1;
}

void day5p2_setup_input(t_intcode_state* state) {
    state->input.data = malloc(sizeof(*state->input.data));
    state->input.size = 1;
    state->input.data[0] = 5;
}

char* day5p1(const char* input) {
    t_intcode_result result = aoc_intcode_eval(input, day5p1_setup_input);

    bigint res = result.state.output.data[result.state.output.head - 1];

    intcode_free_result(&result);
    return aoc_bigint_to_str(res);
}

char* day5p2(const char* input) {
    t_intcode_result result = aoc_intcode_eval(input, day5p2_setup_input);

    bigint res = result.state.output.data[result.state.output.head - 1];

    intcode_free_result(&result);
    return aoc_bigint_to_str(res);
}
