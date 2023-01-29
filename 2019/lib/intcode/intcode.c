#include <stdio.h>
#include <stdlib.h>
#include "stdbool.h"
#include "aoc.h"
#include "intcode.h"

struct s_intcode_op_def {
    t_intcode_op op;
    bool (*eval)(int*, size_t, size_t);
    size_t parameters;
};

static const struct s_intcode_op_def intcode_operations[] = {
    {1, intcode_op_add, 3},
    {2, intcode_op_mul, 3},
};

t_intcode_result aoc_intcode_eval(const char* input,
                                  t_intcode_preprocessing preprocessor) {
    size_t size = 0;
    int* memory = NULL;
    size_t ip = 0;

    aoc_contents_to_ints(input, ',', &memory, &size);

    if (preprocessor) {
        (*preprocessor)(memory, size);
    }

    while (ip < size && memory[ip] != INTCODE_OP_HALT) {
        if (!intcode_eval_opcode(memory, size, &ip)) {
            fprintf(stderr, "intcode failed reading op %d\n", memory[ip]);
            return (t_intcode_result){INTCODE_RESULT_FAILURE, memory, size};
        }
    }

    if (ip >= size) {
        fprintf(stderr, "intcode reached eof without encountering halt\n");
        return (t_intcode_result){INTCODE_RESULT_FAILURE, memory, size};
    }

    return (t_intcode_result){INTCODE_RESULT_OK, memory, size};
}

bool intcode_eval_opcode(int* memory, size_t size, size_t* ip) {
    for (size_t opi = 0; opi < (sizeof intcode_operations / sizeof(*intcode_operations)); opi++) {
        if (memory[*ip] == (int)intcode_operations[opi].op) {
            bool res = (*intcode_operations[opi].eval)(memory, size, *ip);
            if (res)
                *ip += 1 + intcode_operations[opi].parameters;
            return res;
        }
    }
    fprintf(stderr, "unknown intcode op %d\n", memory[*ip]);
    return false;
}
