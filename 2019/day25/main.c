#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

#define DAY25_MANUAL 1
#define DAY25_COMMANDS 2

#define DAY25_CHOICE DAY25_COMMANDS

#define BUFFER_SIZE 1000

static t_intcode_result run_intcode(const char* input) {
    t_intcode_state state = aoc_intcode_boot(input);
    aoc_intcode_upgrade_memory(&state, (1 << 13));

    state.input.data = malloc(sizeof(*state.input.data) * (BUFFER_SIZE + 1));

    return aoc_intcode_eval(state);
}

static void print_output(t_intcode_state* state) {
    for (size_t i = 0; i < state->output.head; i++)
        putchar(state->output.data[i]);
    state->output.head = 0;
}

static void read_user_input(t_bigint_stream* into, FILE* file) {
    char c;

    into->head = 0;
    into->size = 0;

    while (EOF != (c = fgetc(file))) {
        into->data[into->size++] = c;
        if (c == '\n')
            return;
    }
}

static void run_game(const char* input, FILE* stream) {
    t_intcode_result result = run_intcode(input);

    while (result.status == INTCODE_RESULT_FAILURE) {
        print_output(&result.state);
        read_user_input(&result.state.input, stream);
        result = aoc_intcode_eval(result.state);
    }

    print_output(&result.state);
    intcode_free_result(&result);
}

char* day25p1(const char* input) {
#if DAY25_CHOICE == DAY25_COMMANDS
    FILE* file = fopen("day25/commands", "r");
    run_game(input, file);
    fclose(file);
#elif DAY25_CHOICE == DAY25_MANUAL
    run_game(input, stdin);
#endif

    return NULL;
}

char* day25p2(const char* input) {
    // There is no p2
    (void)input;
    return NULL;
}
