#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "aoc.h"
#include "intcode.h"

struct s_intcode_op_def {
    t_intcode_op op;
    bool (*eval)(int*, size_t, size_t);
};

static const struct s_intcode_op_def intcode_operations[] = {
    {1, intcode_op_add},
    {2, intcode_op_mul},
};

t_intcode_result aoc_intcode_eval(const char* input,
                                  t_intcode_preprocessing preprocessor) {
    size_t size = 0;
    int* memory = NULL;
    size_t i = 0;

    aoc_contents_to_ints(input, ',', &memory, &size);

    if (preprocessor) {
        (*preprocessor)(memory, size);
    }

    while (i < size && memory[i] != INTCODE_OP_HALT) {
        if (!intcode_eval_opcode(memory, size, i)) {
            fprintf(stderr, "intcode failed reading op %d\n", memory[i]);
            return (t_intcode_result){INTCODE_RESULT_FAILURE, memory, size};
        }
        i += 4;
    }

    if ((size_t)i >= size) {
        fprintf(stderr, "intcode reached eof without encountering halt\n");
        return (t_intcode_result){INTCODE_RESULT_FAILURE, memory, size};
    }

    return (t_intcode_result){INTCODE_RESULT_OK, memory, size};
}

bool intcode_eval_opcode(int* memory, size_t size, size_t i) {
    for (size_t opi = 0; opi < (sizeof intcode_operations / sizeof(*intcode_operations)); opi++) {
        if (memory[i] == (int)intcode_operations[opi].op) {
            return (*intcode_operations[opi].eval)(memory, size, i);
        }
    }
    fprintf(stderr, "unknown intcode op %d\n", memory[i]);
    return false;
}
