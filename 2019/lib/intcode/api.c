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

void intcode_set_input(t_intcode_state* state, t_bigint_array input) {
    free(state->input.data);
    state->input.data = input.data;
    state->input.size = input.size;
    state->input.head = 0;
}

void intcode_set_input1(t_intcode_state* state, bigint a) {
    free(state->input.data);
    state->input.data = malloc(sizeof(*state->input.data));
    state->input.size = 1;
    state->input.data[0] = a;
    state->input.head = 0;
}

void intcode_set_input2(t_intcode_state* state, bigint a, bigint b) {
    free(state->input.data);
    state->input.data = malloc(sizeof(*state->input.data) * 2);
    state->input.size = 2;
    state->input.data[0] = a;
    state->input.data[1] = b;
    state->input.head = 0;
}

bigint intcode_last_output(t_intcode_result* result) {
    if (result->state.output.data && result->state.output.head > 0)
        return result->state.output.data[result->state.output.head - 1];
    fprintf(stderr, "intcode result has no output\n");
    return 0;
}
