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

static bool receive_packets(t_intcode_state* state, t_packet* packets, size_t size) {
    state->input.head = 0;
    state->input.size = 0;

    if (size > 0) {
        for (size_t i = 0; i < size; i++) {
            state->input.data[state->input.size++] = packets[i].x;
            state->input.data[state->input.size++] = packets[i].y;
        }
        return true;
    } else {
        if (state->input.size == 0 || state->input.data[state->input.size] != -1) {
            state->input.data[state->input.size++] = -1;
            return false;
        }
        return true;
    }
}

static bool send_packets(t_intcode_state* state, t_packet packets[N_COMPUTERS][MAX_PACKETS], size_t size[N_COMPUTERS], t_packet* nat) {
    if (state->output.head == 0)
        return false;

    for (size_t i = 0; (i + 2) < state->output.head; i += 3) {
        int target = state->output.data[i];
        bigint x = state->output.data[i + 1];
        bigint y = state->output.data[i + 2];

        if (target >= 0 && target < N_COMPUTERS) {
            packets[target][size[target]++] = (t_packet){x, y};
        } else {
            nat->x = x;
            nat->y = y;
        }
    }

    state->output.head = 0;
    return true;
}

bigint run_network(const char* input, bool wait_until_duplicate) {
    t_intcode_state computers[N_COMPUTERS];
    t_packet packets[N_COMPUTERS][MAX_PACKETS] = {0};
    t_packet prev_nat = {.x = -1, .y = -1};
    t_packet nat = {.x = -1, .y = -1};
    size_t queue_size[N_COMPUTERS] = {0};

    for (int i = 0; i < N_COMPUTERS; i++)
        computers[i] = run_intcode(input, i);

    for (size_t steps = 0; true; steps++) {
        bool working = false;
        for (int i = 0; i < N_COMPUTERS; i++) {
            working |= send_packets(&computers[i], packets, queue_size, &nat);
            if (nat.x != -1 && nat.y != -1 && !wait_until_duplicate)
                return nat.y;
        }

        for (int i = 0; i < N_COMPUTERS; i++) {
            working |= receive_packets(&computers[i], packets[i], queue_size[i]);
            queue_size[i] = 0;
        }

        if (!working && steps) {
            if (prev_nat.x == nat.x && prev_nat.y == nat.y
                && prev_nat.x != -1 && prev_nat.y != -1) {
                return nat.y;
            }

            prev_nat = nat;
            packets[0][queue_size[0]++] = nat;
            continue;
        }

        for (int i = 0; i < N_COMPUTERS; i++) {
            computers[i] = aoc_intcode_eval(computers[i]).state;
        }
    }
}

char* day23p1(const char* input) {
    return aoc_bigint_to_str(run_network(input, false));
}

char* day23p2(const char* input) {
    return aoc_bigint_to_str(run_network(input, true));
}
