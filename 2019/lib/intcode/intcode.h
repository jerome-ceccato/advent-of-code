#ifndef INTCODE_H
#define INTCODE_H

#include <stdlib.h>
#include <stdbool.h>

typedef enum {
    INTCODE_OP_ADD = 1,
    INTCODE_OP_MUL = 2,

    INTCODE_OP_HALT = 99
} t_intcode_op;

// Helpers
bool intcode_safe_read(int* memory, size_t size, size_t i, int* out);
bool intcode_safe_write(int* memory, size_t size, size_t i, int val);

// Ops
bool intcode_op_add(int* memory, size_t size, size_t i);
bool intcode_op_mul(int* memory, size_t size, size_t i);

// Internals
bool intcode_eval_opcode(int* memory, size_t size, size_t i);

#endif
