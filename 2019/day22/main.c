#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

#define DECK_SIZE 10007

static void deal_new(int cards[DECK_SIZE]) {
    for (int i = 0; i < (DECK_SIZE / 2); i++) {
        int j = DECK_SIZE - 1 - i;
        int tmp = cards[i];
        cards[i] = cards[j];
        cards[j] = tmp;
    }
}

static void cut(int cards[DECK_SIZE], int n) {
    int copy[DECK_SIZE];
    memcpy(copy, cards, sizeof(*cards) * DECK_SIZE);

    if (n > 0) {
        memcpy(cards, copy + n, sizeof(*cards) * (DECK_SIZE - n));
        memcpy(cards + (DECK_SIZE - n), copy, sizeof(*cards) * n);
    } else {
        n = -n;
        memcpy(cards, copy + (DECK_SIZE - n), sizeof(*cards) * n);
        memcpy(cards + n, copy, sizeof(*cards) * (DECK_SIZE - n));
    }
}

static void deal_with_increment(int cards[DECK_SIZE], int inc) {
    int copy[DECK_SIZE];
    memcpy(copy, cards, sizeof(*cards) * DECK_SIZE);

    for (int i = 0; i < DECK_SIZE; i++) {
        cards[(i * inc) % DECK_SIZE] = copy[i];
    }
}

static bool has_prefix(const char *str, const char *prefix) {
    size_t str_sz = strlen(str);
    size_t prefix_sz = strlen(prefix);

    if (str_sz >= prefix_sz)
        return strncmp(prefix, str, prefix_sz) == 0;
    return false;
}

static void shuffle(const char *input, int cards[DECK_SIZE]) {
    while (input) {
        if (has_prefix(input, "deal into new stack")) {
            deal_new(cards);
        } else if (has_prefix(input, "cut ")) {
            int n = strtol(input + strlen("cut "), NULL, 10);
            cut(cards, n);
        } else if (has_prefix(input, "deal with increment ")) {
            int n = strtol(input + strlen("deal with increment "), NULL, 10);
            deal_with_increment(cards, n);
        } else {
            fprintf(stderr, "Unrecognized action %s\n", input);
        }

        const char *p = strchr(input, '\n');
        input = p ? p + 1 : NULL;
    }
}

char *day22p1(const char *input) {
    int cards[DECK_SIZE];

    for (int i = 0; i < DECK_SIZE; i++)
        cards[i] = i;

    shuffle(input, cards);

    for (int i = 0; i < DECK_SIZE; i++)
        if (cards[i] == 2019)
            return aoc_asprintf("%d", i);
    return NULL;
}

char *day22p2(const char *input) {
    return NULL;
}
