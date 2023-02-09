#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

#define N_COMPUTERS 50
#define MAX_PACKETS 1000

typedef struct {
    bigint x;
    bigint y;
} t_packet;

static t_intcode_state run_intcode(const char* input, int address) {
    t_intcode_state state = aoc_intcode_boot(input);
    aoc_intcode_upgrade_memory(&state, 4096);

    state.input.data = malloc(sizeof(*state.input.data) * (MAX_PACKETS * 3));
    state.input.size = 1;
    state.input.data[0] = address;
    state.input.head = 0;

    return aoc_intcode_eval(state).state;
}

static void receive_packets(t_intcode_state* state, t_packet* packets, size_t size) {
    state->input.head = 0;
    state->input.size = 0;

    if (size > 0)
        for (size_t i = 0; i < size; i++) {
            state->input.data[state->input.size++] = packets[i].x;
            state->input.data[state->input.size++] = packets[i].y;
        }
    else
        state->input.data[state->input.size++] = -1;
}

static bool send_packets(t_intcode_state* state, t_packet packets[N_COMPUTERS][MAX_PACKETS], size_t size[N_COMPUTERS], bigint* out) {
    for (size_t i = 0; (i + 2) < state->output.head; i += 3) {
        int target = state->output.data[i];
        bigint x = state->output.data[i + 1];
        bigint y = state->output.data[i + 2];

        if (target >= 0 && target < N_COMPUTERS) {
            // printf("#%d sends packet to %d\n", address, target);
            packets[target][size[target]++] = (t_packet){x, y};
        } else {
            printf("Trying to send packet to %d (%s, %s)\n", target, aoc_bigint_to_str(x), aoc_bigint_to_str(y));
            if (target == 255) {
                *out = y;
                return true;
            }
        }
    }

    state->output.head = 0;
    return false;
}

char* day23p1(const char* input) {
    t_intcode_state computers[N_COMPUTERS];
    t_packet packets[N_COMPUTERS][MAX_PACKETS] = {0};
    size_t queue_size[N_COMPUTERS] = {0};

    for (int i = 0; i < N_COMPUTERS; i++)
        computers[i] = run_intcode(input, i);

    while (true) {
        for (int i = 0; i < N_COMPUTERS; i++) {
            bigint out;
            if (send_packets(&computers[i], packets, queue_size, &out))
                return aoc_bigint_to_str(out);
        }

        for (int i = 0; i < N_COMPUTERS; i++) {
            // if (queue_size[i] > 0)
            //     printf("#%d receives %zu packet\n", i, queue_size[i]);
            receive_packets(&computers[i], packets[i], queue_size[i]);
            queue_size[i] = 0;
        }

        for (int i = 0; i < N_COMPUTERS; i++) {
            computers[i] = aoc_intcode_eval(computers[i]).state;
        }
    }

    return NULL;
}

char* day23p2(const char* input) {
    return NULL;
}
