#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "intcode.h"

bool intcode_op_add(t_intcode_state* state) {
    int lhs, rhs;
    int op = state->memory.data[state->ip];

    return (state->ip + 3) < state->memory.size
           && intcode_safe_read(state, state->memory.data[state->ip + 1], intcode_get_mode(op, 1), &lhs)
           && intcode_safe_read(state, state->memory.data[state->ip + 2], intcode_get_mode(op, 2), &rhs)
           && intcode_safe_write(state, state->memory.data[state->ip + 3], intcode_get_mode(op, 3), lhs + rhs);
}

bool intcode_op_mul(t_intcode_state* state) {
    int lhs, rhs;
    int op = state->memory.data[state->ip];

    return (state->ip + 3) < state->memory.size
           && intcode_safe_read(state, state->memory.data[state->ip + 1], intcode_get_mode(op, 1), &lhs)
           && intcode_safe_read(state, state->memory.data[state->ip + 2], intcode_get_mode(op, 2), &rhs)
           && intcode_safe_write(state, state->memory.data[state->ip + 3], intcode_get_mode(op, 3), lhs * rhs);
}

bool intcode_op_read(t_intcode_state* state) {
    int input;
    int op = state->memory.data[state->ip];

    return (state->ip + 1) < state->memory.size
           && intcode_safe_read_input(state, &input)
           && intcode_safe_write(state, state->memory.data[state->ip + 1], intcode_get_mode(op, 1), input);
}

bool intcode_op_write(t_intcode_state* state) {
    int output;
    int op = state->memory.data[state->ip];

    return (state->ip + 1) < state->memory.size
           && intcode_safe_read(state, state->memory.data[state->ip + 1], intcode_get_mode(op, 1), &output)
           && intcode_safe_write_output(state, output);
}

bool _intcode_jump(t_intcode_state* state, bool expected) {
    int test, val;
    int op = state->memory.data[state->ip];

    if ((state->ip + 2) < state->memory.size
        && intcode_safe_read(state, state->memory.data[state->ip + 1], intcode_get_mode(op, 1), &test)
        && intcode_safe_read(state, state->memory.data[state->ip + 2], intcode_get_mode(op, 2), &val)) {
        if (!!test == expected)
            state->ip = val;
        else
            state->ip += 3;
        return true;
    }
    return false;
}

bool intcode_op_jump_t(t_intcode_state* state) {
    return _intcode_jump(state, true);
}

bool intcode_op_jump_f(t_intcode_state* state) {
    return _intcode_jump(state, false);
}

enum {
    INTCODE_CMP_LT,
    INTCODE_CMP_EQ
};

int _intcode_cmp_operation(int operation, int lhs, int rhs) {
    switch (operation) {
        case INTCODE_CMP_LT:
            return lhs < rhs;
        case INTCODE_CMP_EQ:
            return lhs == rhs;
    }
    fprintf(stderr, "intcode invalid cmp %d\n", operation);
    return 0;
}

bool _intcode_cmp(t_intcode_state* state, int operation) {
    int lhs, rhs;
    int op = state->memory.data[state->ip];

    return (state->ip + 3) < state->memory.size
           && intcode_safe_read(state, state->memory.data[state->ip + 1], intcode_get_mode(op, 1), &lhs)
           && intcode_safe_read(state, state->memory.data[state->ip + 2], intcode_get_mode(op, 2), &rhs)
           && intcode_safe_write(state, state->memory.data[state->ip + 3], intcode_get_mode(op, 3), _intcode_cmp_operation(operation, lhs, rhs));
}

bool intcode_op_lt(t_intcode_state* state) {
    return _intcode_cmp(state, INTCODE_CMP_LT);
}

bool intcode_op_eq(t_intcode_state* state) {
    return _intcode_cmp(state, INTCODE_CMP_EQ);
}

bool intcode_op_adjust_relative_base(t_intcode_state* state) {
    int offset;
    int op = state->memory.data[state->ip];

    if ((state->ip + 1) < state->memory.size
        && intcode_safe_read(state, state->memory.data[state->ip + 1], intcode_get_mode(op, 1), &offset)) {
        state->relative_base += offset;
        return true;
    }
    return false;
}