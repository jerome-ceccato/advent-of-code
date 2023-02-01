#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "intcode.h"
#include "aoc.h"

void intcode_free_result(t_intcode_result* res) {
    if (res) {
        free(res->state.memory.data);
        free(res->state.input.data);
        free(res->state.output.data);
    }
}

void intcode_print_output(const t_intcode_state* state) {
    for (size_t i = 0; i < state->output.head; i++)
        printf(BIGINT_FMT "%s", state->output.data[i], (i + 1 < state->output.head) ? " " : "\n");
}

static t_bigint_array _intcode_input_seed;
static void _seed_input(t_intcode_state* state) {
    free(state->input.data);
    state->input.data = _intcode_input_seed.data;
    state->input.size = _intcode_input_seed.size;
    state->input.head = 0;

    _intcode_input_seed = (t_bigint_array){NULL, 0};
}

t_intcode_preprocessing intcode_seed_input(t_bigint_array input) {
    free(_intcode_input_seed.data);
    _intcode_input_seed = input;
    return _seed_input;
}

t_intcode_preprocessing intcode_seed_input1(bigint a) {
    free(_intcode_input_seed.data);
    _intcode_input_seed.data = malloc(sizeof(*_intcode_input_seed.data));
    _intcode_input_seed.size = 1;
    _intcode_input_seed.data[0] = a;
    return _seed_input;
}

t_intcode_preprocessing intcode_seed_input2(bigint a, bigint b) {
    free(_intcode_input_seed.data);
    _intcode_input_seed.data = malloc(sizeof(*_intcode_input_seed.data) * 2);
    _intcode_input_seed.size = 2;
    _intcode_input_seed.data[0] = a;
    _intcode_input_seed.data[1] = b;
    return _seed_input;
}
