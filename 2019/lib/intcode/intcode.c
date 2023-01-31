#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stdbool.h"
#include "aoc.h"
#include "intcode.h"

struct s_intcode_op_def {
    t_intcode_op op;
    bool (*eval)(t_intcode_state*);
    size_t ip_diff;
};

static const struct s_intcode_op_def intcode_operations[] = {
    {INTCODE_OP_ADD, intcode_op_add, 3 + 1},
    {INTCODE_OP_MUL, intcode_op_mul, 3 + 1},
    {INTCODE_OP_READ, intcode_op_read, 1 + 1},
    {INTCODE_OP_WRITE, intcode_op_write, 1 + 1},
    {INTCODE_OP_JUMP_TRUE, intcode_op_jump_t, 0},
    {INTCODE_OP_JUMP_FALSE, intcode_op_jump_f, 0},
    {INTCODE_OP_LESS_THAN, intcode_op_lt, 3 + 1},
    {INTCODE_OP_EQUALS, intcode_op_eq, 3 + 1},
};

t_intcode_state intcode_make_state() {
    t_intcode_state state;
    memset(&state, 0, sizeof state);
    return state;
}

t_intcode_result aoc_intcode_eval(const char* input,
                                  t_intcode_preprocessing preprocessor) {
    t_intcode_state state = intcode_make_state();

    aoc_contents_to_ints(input, ',', &state.memory.data, &state.memory.size);

    if (preprocessor) {
        (*preprocessor)(&state);
    }

    return aoc_intcode_restart(state);
}

t_intcode_result aoc_intcode_restart(t_intcode_state state) {
    while (state.ip < state.memory.size && state.memory.data[state.ip] != INTCODE_OP_HALT) {
        if (!intcode_eval_opcode(&state)) {
            // Failing a read input op is expected when waiting for an input from somewhere else
            if ((state.memory.data[state.ip] % 100) != INTCODE_OP_READ)
                fprintf(stderr, "intcode failed reading op %d\n", state.memory.data[state.ip]);
            return (t_intcode_result){INTCODE_RESULT_FAILURE, state};
        }
    }

    if (state.ip >= state.memory.size) {
        fprintf(stderr, "intcode reached eof without encountering halt\n");
        return (t_intcode_result){INTCODE_RESULT_FAILURE, state};
    }

    return (t_intcode_result){INTCODE_RESULT_OK, state};
}

bool intcode_eval_opcode(t_intcode_state* state) {
    int opcode = state->memory.data[state->ip] % 100;
    for (size_t opi = 0; opi < (sizeof intcode_operations / sizeof(*intcode_operations)); opi++) {
        if (opcode == (int)intcode_operations[opi].op) {
            bool res = (*intcode_operations[opi].eval)(state);
            if (res)
                state->ip += intcode_operations[opi].ip_diff;
            return res;
        }
    }
    fprintf(stderr, "unknown intcode op %d\n", state->memory.data[state->ip]);
    return false;
}
