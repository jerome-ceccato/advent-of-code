#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "intcode.h"
#include "aoc.h"

int intcode_get_mode(int op, int pos) {
    op /= 100;
    while (pos-- != 0) {
        op /= 10;
    }
    return op % 10;
}

bool intcode_safe_read(t_intcode_state* state, int param, int mode, int* out) {
    if (mode == 0) {
        if ((size_t)param < state->memory.size) {
            *out = state->memory.data[param];
            return true;
        } else {
            fprintf(stderr, "intcode attempting to read oob %d\n", param);
            return false;
        }
    } else if (mode == 1) {
        *out = param;
        return true;
    } else {
        fprintf(stderr, "intcode unsupported mode %d\n", mode);
        return false;
    }
}

bool intcode_safe_write(t_intcode_state* state, size_t addr, int val) {
    if (addr < state->memory.size) {
        state->memory.data[addr] = val;
        return true;
    }
    fprintf(stderr, "intcode attempting to write oob %zu\n", addr);
    return false;
}

bool intcode_safe_read_input(t_intcode_state* state, int* out) {
    if (state->input.head < state->input.size) {
        *out = state->input.data[state->input.head++];
        return true;
    }
    fprintf(stderr, "intcode attempting to read empty input\n");
    return false;
}

bool intcode_safe_write_output(t_intcode_state* state, int value) {
    if (state->output.data == NULL) {
        state->output.size = 16;
        state->output.data = malloc(state->output.size * sizeof(*state->output.data));
    } else if (state->output.head >= state->output.size) {
        state->output.size *= 2;
        state->output.data = realloc(state->output.data, state->output.size * sizeof(*state->output.data));
    }

    state->output.data[state->output.head++] = value;
    return true;
}

void intcode_free_result(t_intcode_result* res) {
    if (res) {
        free(res->state.memory.data);
        free(res->state.input.data);
        free(res->state.output.data);
    }
}
