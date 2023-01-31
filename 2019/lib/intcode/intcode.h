#ifndef INTCODE_H
#define INTCODE_H

#include <stdlib.h>
#include <stdbool.h>

typedef enum {
    INTCODE_OP_ADD = 1,
    INTCODE_OP_MUL = 2,
    INTCODE_OP_READ = 3,
    INTCODE_OP_WRITE = 4,
    INTCODE_OP_JUMP_TRUE = 5,
    INTCODE_OP_JUMP_FALSE = 6,
    INTCODE_OP_LESS_THAN = 7,
    INTCODE_OP_EQUALS = 8,

    INTCODE_OP_HALT = 99
} t_intcode_op;

typedef enum {
    INTCODE_RESULT_OK,
    INTCODE_RESULT_FAILURE
} t_intcode_result_status;

// An int array with its size
typedef struct {
    int* data;
    size_t size;
} t_int_array;

// Like an int array, but keeping track of its current position
typedef struct {
    int* data;
    size_t head;
    size_t size;
} t_int_stream;

// All data representing the intcode computer
typedef struct {
    t_int_array memory;  // Current memory
    size_t ip;           // Instruction pointer

    t_int_stream input;   // Input, should be set during preprocessing
    t_int_stream output;  // Output, will be dynamically created as needed
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

// Cleans up the memory allocated inside a `t_intcode_result`
void intcode_free_result(t_intcode_result* res);

/*
 * Private
 */

// Helpers
int intcode_get_mode(int op, int pos);  // pos is the position of the parameter, starts at 0

bool intcode_safe_read(t_intcode_state* state, int param, int mode, int* out);
bool intcode_safe_write(t_intcode_state* state, size_t addr, int val);
bool intcode_safe_read_input(t_intcode_state* state, int* out);
bool intcode_safe_write_output(t_intcode_state* state, int value);

// Ops
bool intcode_op_add(t_intcode_state* state);
bool intcode_op_mul(t_intcode_state* state);
bool intcode_op_read(t_intcode_state* state);
bool intcode_op_write(t_intcode_state* state);
bool intcode_op_jump_t(t_intcode_state* state);
bool intcode_op_jump_f(t_intcode_state* state);
bool intcode_op_lt(t_intcode_state* state);
bool intcode_op_eq(t_intcode_state* state);

// Internals
bool intcode_eval_opcode(t_intcode_state* state);

#endif
