#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "intcode.h"
#include "aoc.h"

t_intcode_param_mode intcode_get_mode(int op, int pos) {
    op /= 10;  // Assumes pos starts at 1 for symetry with accessing parameters (ip + pos)
    while (pos-- > 0)
        op /= 10;
    return op % 10;
}

bool intcode_safe_read(t_intcode_state* state, int param, t_intcode_param_mode mode, int* out) {
    switch (mode) {
        case INTCODE_PARAM_MODE_POSITION:
            if (param >= 0 && (size_t)param < state->memory.size) {
                *out = state->memory.data[param];
                return true;
            } else {
                fprintf(stderr, "intcode attempting to read oob %d\n", param);
                return false;
            }
        case INTCODE_PARAM_MODE_IMMEDIATE:
            *out = param;
            return true;
        case INTCODE_PARAM_MODE_RELATIVE:
            return intcode_safe_read(state, param + state->relative_base, INTCODE_PARAM_MODE_POSITION, out);
    }

    fprintf(stderr, "intcode_safe_read unsupported mode %d\n", mode);
    return false;
}

bool intcode_safe_write(t_intcode_state* state, int addr, t_intcode_param_mode mode, int val) {
    switch (mode) {
        case INTCODE_PARAM_MODE_POSITION:
            if (addr >= 0 && (size_t)addr < state->memory.size) {
                state->memory.data[addr] = val;
                return true;
            } else {
                fprintf(stderr, "intcode attempting to write oob %d\n", addr);
                return false;
            }
        case INTCODE_PARAM_MODE_IMMEDIATE:
            fprintf(stderr, "intcode unsupported immediate mode for write op\n");
            return false;
        case INTCODE_PARAM_MODE_RELATIVE:
            return intcode_safe_write(state, addr + state->relative_base, INTCODE_PARAM_MODE_POSITION, val);
    }

    fprintf(stderr, "intcode_safe_write unsupported mode %d\n", mode);
    return false;
}

bool intcode_safe_read_input(t_intcode_state* state, int* out) {
    if (state->input.head < state->input.size) {
        *out = state->input.data[state->input.head++];
        return true;
    }
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

void intcode_print_output(const t_intcode_state* state) {
    for (size_t i = 0; i < state->output.head; i++)
        printf("%d%s", state->output.data[i], (i + 1 < state->output.head) ? " " : "\n");
}
