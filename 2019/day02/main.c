#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"
#include "utils.h"

static t_intcode_result run_intcode(const char* input, int noun, int verb) {
    t_intcode_state state = aoc_intcode_boot(input);
    state.memory.data[1] = noun;
    state.memory.data[2] = verb;

    return aoc_intcode_eval(state);
}

char* day2p1(const char* input) {
    t_intcode_result result = run_intcode(input, 12, 2);
    bigint res = result.state.memory.data[0];

    intcode_free_result(&result);
    return aoc_bigint_to_str(res);
}

char* day2p2(const char* input) {
    for (int noun = 0; noun < 100; noun++) {
        for (int verb = 0; verb < 100; verb++) {
            t_intcode_result result = run_intcode(input, noun, verb);
            bigint ouput = result.state.memory.data[0];
            intcode_free_result(&result);

            if (ouput == 19690720) {
                return aoc_asprintf("%d", 100 * noun + verb);
            }
        }
    }
    return NULL;
}
