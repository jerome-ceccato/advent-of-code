#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"
#include "utils.h"

// This preprocessor function was a terrible idea
static int _d7_amp_setting;
static int _d7_amp_input;
void _day7_setup_input(t_intcode_state* state) {
    state->input.data = malloc(sizeof(*state->input.data) * 2);
    state->input.size = 2;
    state->input.data[0] = _d7_amp_setting;
    state->input.data[1] = _d7_amp_input;
}

void (*day7_setup_input_with(int amp_setting, int amp_input))(t_intcode_state*) {
    _d7_amp_setting = amp_setting;
    _d7_amp_input = amp_input;
    return _day7_setup_input;
}

int day7_max_signal(const char* input, int in, int used) {
    if (used == 0b11111) {
        return in;
    }

    int max_sig = 0;
    for (int i = 0; i < 5; i++) {
        if (!(used & (1 << i))) {
            t_intcode_result result = aoc_intcode_eval(input, day7_setup_input_with(i, in));
            int output = result.state.output.data[0];
            intcode_free_result(&result);

            int next = day7_max_signal(input, output, used | (1 << i));
            max_sig = max(next, max_sig);
        }
    }
    return max_sig;
}

int day7_sequence_to_signal(const char* input, int* sequence) {
    t_intcode_result states[5];

    int output = 0;
    for (int i = 0; i < 5; i++) {
        states[i] = aoc_intcode_eval(input, day7_setup_input_with(sequence[i], output));
        output = states[i].state.output.data[states[i].state.output.head - 1];
    }

    while (states[4].status == INTCODE_RESULT_FAILURE) {
        for (int i = 0; i < 5; i++) {
            states[i].state.input.data[0] = output;
            states[i].state.input.head = 0;
            states[i].state.input.size = 1;

            states[i] = aoc_intcode_restart(states[i].state);
            output = states[i].state.output.data[states[i].state.output.head - 1];
        }
    }
    return output;
}

int day7_max_feedback_signal(const char* input, int* sequence, int si, int used) {
    if (used == 0b11111) {
        return day7_sequence_to_signal(input, sequence);
    }

    int max_sig = 0;
    for (int i = 0; i < 5; i++) {
        if (!(used & (1 << i))) {
            sequence[si] = i + 5;
            int next = day7_max_feedback_signal(input, sequence, si + 1, used | (1 << i));
            max_sig = max(next, max_sig);
        }
    }
    return max_sig;
}

char* day7p1(const char* input) {
    return aoc_asprintf("%d", day7_max_signal(input, 0, 0));
}

char* day7p2(const char* input) {
    int seq[5] = {0};
    return aoc_asprintf("%d", day7_max_feedback_signal(input, seq, 0, 0));
}
