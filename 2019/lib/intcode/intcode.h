#ifndef INTCODE_H
#define INTCODE_H

#include <stdlib.h>
#include <stdbool.h>
#include "utils.h"

typedef enum {
    INTCODE_OP_ADD = 1,
    INTCODE_OP_MUL = 2,
    INTCODE_OP_READ = 3,
    INTCODE_OP_WRITE = 4,
    INTCODE_OP_JUMP_TRUE = 5,
    INTCODE_OP_JUMP_FALSE = 6,
    INTCODE_OP_LESS_THAN = 7,
    INTCODE_OP_EQUALS = 8,
    INTCODE_OP_ADJUST_REL_BASE = 9,

    INTCODE_OP_HALT = 99
} t_intcode_op;

typedef enum {
    INTCODE_PARAM_MODE_POSITION = 0,
    INTCODE_PARAM_MODE_IMMEDIATE = 1,
    INTCODE_PARAM_MODE_RELATIVE = 2
} t_intcode_param_mode;

typedef enum {
    INTCODE_RESULT_OK,
    INTCODE_RESULT_FAILURE
} t_intcode_result_status;

// An bigint array with its size
typedef struct {
    bigint* data;
    size_t size;
} t_bigint_array;

// Like a bigint array, but keeping track of its current position
typedef struct {
    bigint* data;
    size_t head;
    size_t size;
} t_bigint_stream;

// All data representing the intcode computer
typedef struct {
    t_bigint_array memory;  // Current memory
    size_t ip;              // Instruction pointer
    bigint relative_base;   // For relative parameter mode

    t_bigint_stream input;   // Input, should be set during preprocessing
    t_bigint_stream output;  // Output, will be dynamically created as needed
} t_intcode_state;

typedef struct {
    t_intcode_result_status status;  // Whether or not the program finished correctly
    t_intcode_state state;           // The final state of the incode program
} t_intcode_result;

typedef void (*t_intcode_preprocessing)(t_intcode_state*);

// Evals an intcode program from a string
// `preprocessor` is an optional function that runs on the intcode memory before
// evaluation
t_intcode_result aoc_intcode_eval(const char* input,
                                  t_intcode_preprocessing preprocessor);

// Restarts evaluation of an intcode program that failed (e.g. because it was waiting for an input)
t_intcode_result aoc_intcode_restart(t_intcode_state state);

// Cleans up the memory allocated inside a `t_intcode_result`
void intcode_free_result(t_intcode_result* res);

/*
 * Private
 */

// Helpers
t_intcode_param_mode intcode_get_mode(bigint op, int pos);  // pos is the position of the parameter, starts at 1

bool intcode_safe_read(t_intcode_state* state, bigint param, t_intcode_param_mode mode, bigint* out);
bool intcode_safe_write(t_intcode_state* state, bigint addr, t_intcode_param_mode mode, bigint val);
bool intcode_safe_read_input(t_intcode_state* state, bigint* out);
bool intcode_safe_write_output(t_intcode_state* state, bigint value);

void intcode_print_output(const t_intcode_state* state);

// Ops
bool intcode_op_add(t_intcode_state* state);
bool intcode_op_mul(t_intcode_state* state);
bool intcode_op_read(t_intcode_state* state);
bool intcode_op_write(t_intcode_state* state);
bool intcode_op_jump_t(t_intcode_state* state);
bool intcode_op_jump_f(t_intcode_state* state);
bool intcode_op_lt(t_intcode_state* state);
bool intcode_op_eq(t_intcode_state* state);
bool intcode_op_adjust_relative_base(t_intcode_state* state);

// Internals
bool intcode_eval_opcode(t_intcode_state* state);

#endif
