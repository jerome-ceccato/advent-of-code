#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "intcode.h"

bool intcode_op_add(int* memory, size_t size, size_t i) {
    int lhs, rhs;
    return (i + 3) < size
           && intcode_safe_read(memory, size, memory[i + 1], &lhs)
           && intcode_safe_read(memory, size, memory[i + 2], &rhs)
           && intcode_safe_write(memory, size, memory[i + 3], lhs + rhs);
}

bool intcode_op_mul(int* memory, size_t size, size_t i) {
    int lhs, rhs;
    return (i + 3) < size
           && intcode_safe_read(memory, size, memory[i + 1], &lhs)
           && intcode_safe_read(memory, size, memory[i + 2], &rhs)
           && intcode_safe_write(memory, size, memory[i + 3], lhs * rhs);
}
