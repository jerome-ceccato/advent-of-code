#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"
#include "utils.h"

// There are no closures in C, so we improvise...
static int _day2_noun;
static int _day2_verb;
void _day2_setup_memory(t_intcode_state* state) {
    state->memory.data[1] = _day2_noun;
    state->memory.data[2] = _day2_verb;
}

void (*day2_setup_memory_with(int noun, int verb))(t_intcode_state*) {
    _day2_noun = noun;
    _day2_verb = verb;
    return _day2_setup_memory;
}

char* day2p1(const char* input) {
    t_intcode_result result = aoc_intcode_eval(input, day2_setup_memory_with(12, 2));

    bigint res = result.state.memory.data[0];

    intcode_free_result(&result);
    return aoc_bigint_to_str(res);
}

char* day2p2(const char* input) {
    for (int noun = 0; noun < 100; noun++) {
        for (int verb = 0; verb < 100; verb++) {
            t_intcode_result result = aoc_intcode_eval(input, day2_setup_memory_with(noun, verb));
            bigint ouput = result.state.memory.data[0];
            intcode_free_result(&result);

            if (ouput == 19690720) {
                return aoc_asprintf("%d", 100 * noun + verb);
            }
        }
    }
    return NULL;
}
