#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"
#include "utils.h"

static t_intcode_result run_intcode(const char* input, bigint setting, bigint signal) {
    t_intcode_state state = aoc_intcode_boot(input);
    intcode_set_input2(&state, setting, signal);

    return aoc_intcode_eval(state);
}

bigint day7_max_signal(const char* input, bigint in, int used) {
    if (used == 0b11111) {
        return in;
    }

    int max_sig = 0;
    for (int i = 0; i < 5; i++) {
        if (!(used & (1 << i))) {
            t_intcode_result result = run_intcode(input, i, in);
            bigint output = result.state.output.data[0];
            intcode_free_result(&result);

            int next = day7_max_signal(input, output, used | (1 << i));
            max_sig = max(next, max_sig);
        }
    }
    return max_sig;
}

bigint day7_sequence_to_signal(const char* input, int* sequence) {
    t_intcode_result states[5];

    bigint output = 0;
    // Do the loop once to instantiate the intcode states
    for (int i = 0; i < 5; i++) {
        states[i] = run_intcode(input, sequence[i], output);
        output = states[i].state.output.data[states[i].state.output.head - 1];
    }

    // As long as execution didn't finish normally, loop again by replacing the input of each intcode states
    // and restarting the program
    while (states[4].status == INTCODE_RESULT_FAILURE) {
        for (int i = 0; i < 5; i++) {
            states[i].state.input.data[0] = output;
            states[i].state.input.head = 0;
            states[i].state.input.size = 1;

            states[i] = aoc_intcode_eval(states[i].state);
            output = states[i].state.output.data[states[i].state.output.head - 1];
        }
    }

    for (int i = 0; i < 5; i++)
        intcode_free_result(states + i);
    return output;
}

bigint day7_max_feedback_signal(const char* input, int* sequence, int si, int used) {
    if (used == 0b11111) {
        return day7_sequence_to_signal(input, sequence);
    }

    bigint max_sig = 0;
    for (int i = 0; i < 5; i++) {
        if (!(used & (1 << i))) {
            sequence[si] = i + 5;
            bigint next = day7_max_feedback_signal(input, sequence, si + 1, used | (1 << i));
            max_sig = max(next, max_sig);
        }
    }
    return max_sig;
}

char* day7p1(const char* input) {
    return aoc_bigint_to_str(day7_max_signal(input, 0, 0));
}

char* day7p2(const char* input) {
    int seq[5] = {0};
    return aoc_bigint_to_str(day7_max_feedback_signal(input, seq, 0, 0));
}
