#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

static t_intcode_result run_intcode(const char* input, const char* program) {
    t_intcode_state state = aoc_intcode_boot(input);
    aoc_intcode_upgrade_memory(&state, 4096);

    t_bigint_array in;
    in.size = strlen(program);
    in.data = malloc(sizeof(*in.data) * in.size);
    for (int i = 0; program[i]; i++)
        in.data[i] = program[i];
    intcode_set_input(&state, in);

    return aoc_intcode_eval(state);
}

char* day21p1(const char* input) {
    // If there's a hole 1-3 blocks ahead and the 4th block is ground, jump
    const char* program =
        "NOT A J\n"
        "OR J T\n"  // T = !A

        "NOT B J\n"
        "OR J T\n"  // T |= !B

        "NOT C J\n"
        "OR J T\n"  // T |= !C

        "NOT D J\n"
        "NOT J J\n"  // J = D

        "AND T J\n"  // J &= T

        "WALK\n";
    t_intcode_result result = run_intcode(input, program);

    printf("result: %d\n", result.status);
    for (size_t i = 0; i < result.state.output.head; i++)
        putchar(result.state.output.data[i]);

    bigint damage = intcode_last_output(&result);

    intcode_free_result(&result);
    return aoc_bigint_to_str(damage);
}

char* day21p2(const char* input) {
    const char* program =
        // if (D && !C && (E || H))
        "NOT C T\n"
        "AND D T\n"
        "OR E J\n"
        "OR H J\n"
        "AND T J\n"

        // || !B && !E && D
        // translated into || !(!D || E || B)
        "NOT D T\n"
        "OR E T\n"
        "OR B T\n"
        "NOT T T\n"
        "OR T J\n"

        // || !A -> jmp
        "NOT A T\n"
        "OR T J\n"
        "RUN\n";
    t_intcode_result result = run_intcode(input, program);

    for (size_t i = 0; i < result.state.output.head; i++)
        putchar(result.state.output.data[i]);

    bigint damage = intcode_last_output(&result);

    intcode_free_result(&result);
    return aoc_bigint_to_str(damage);
}
