#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

typedef struct {
    size_t amount;
    char res[100];
} t_res_cost;

typedef struct {
    t_res_cost cost[100];
    size_t ncost;
    t_res_cost result;
} t_line;

// Parse the input into temporary data struct, to make building our maps easier
static size_t parse_all(const char* input, t_line tentative[100]) {
    size_t line = 0;
    while (*input) {
        size_t item = 0;
        do {
            if (*input == ',')
                input++;

            tentative[line].cost[item].amount = strtol(input, (char**)&input, 10);
            input++;

            int bufi = 0;
            while (isalpha(*input))
                tentative[line].cost[item].res[bufi++] = *(input++);
            tentative[line].cost[item].res[bufi] = 0;

            item++;
        } while (*input == ',');
        tentative[line].ncost = item;

        input += strlen(" => ");
        tentative[line].result.amount = strtol(input, (char**)&input, 10);
        input++;

        int bufi = 0;
        while (isalpha(*input))
            tentative[line].result.res[bufi++] = *(input++);
        tentative[line].result.res[bufi] = 0;

        if (*input == '\n')
            input++;
        line++;
    }

    return line;
}

// List all unique identifiers
static char** find_identifiers(t_line lines[100], size_t size) {
    char** ids = malloc(sizeof(*ids) * (size + 1));
    size_t i = 0;

    ids[i++] = strdup("FUEL");
    ids[i++] = strdup("ORE");
    for (size_t j = 0; j < size; j++) {
        if (strcmp(lines[j].result.res, "FUEL"))
            ids[i++] = strdup(lines[j].result.res);
    }

    return ids;
}

static size_t identifier_idx(char** identifiers, size_t size, char* search) {
    for (size_t i = 0; i < size; i++) {
        if (!strcmp(identifiers[i], search)) {
            return i;
        }
    }
    exit(1);
}

// [Id][Cost], [x][size] is the number of x produced per cost
static int** build_cost_map(t_line lines[100], char** identifiers, size_t size) {
    int** map = malloc(sizeof(*map) * size);
    for (size_t i = 0; i < size; i++)
        map[i] = calloc(size + 1, sizeof(**map));

    for (size_t linei = 0; linei < (size - 1); linei++) {
        size_t id = identifier_idx(identifiers, size, lines[linei].result.res);

        map[id][size] = lines[linei].result.amount;
        for (size_t costi = 0; costi < lines[linei].ncost; costi++) {
            map[id][identifier_idx(identifiers, size, lines[linei].cost[costi].res)] = lines[linei].cost[costi].amount;
        }
    }

    return map;
}

// Returns the amount of ORE needed to buy the given item,
// updating bank along the way if any unused items are produced
static bigint buy(int** cost_map, bigint* bank, size_t size, int target, bigint quantity) {
    //  Use banked items if possible
    if (bank[target]) {
        if (quantity <= bank[target]) {
            bank[target] -= quantity;
            return 0;
        }
        quantity -= bank[target];
        bank[target] = 0;
    }

    // Determine puchase amount
    int produced_per_cost = cost_map[target][size];
    bigint pamount = quantity / produced_per_cost + !!(quantity % produced_per_cost);

    bigint total = 0;
    // Buy all requirements
    for (size_t i = 0; i < size; i++) {
        if (cost_map[target][i]) {
            if (i == 1) {  // ORE
                total += pamount * cost_map[target][i];
            } else {
                total += buy(cost_map, bank, size, i, pamount * cost_map[target][i]);
            }
        }
    }

    // Bank any leftovers
    bigint remaining = (pamount * produced_per_cost) - quantity;
    bank[target] += remaining;

    return total;
}

char* day14p1(const char* input) {
    t_line tmp[100];
    size_t nline = parse_all(input, tmp);

    size_t id_size = nline + 1;
    char** identifiers = find_identifiers(tmp, id_size);

    int** cost_map = build_cost_map(tmp, identifiers, id_size);

    bigint* bank = calloc(id_size, sizeof(*bank));
    bigint total = buy(cost_map, bank, id_size, 0, 1);

    free_pp((void**)identifiers, id_size);
    free_pp((void**)cost_map, id_size);
    free(bank);

    return aoc_bigint_to_str(total);
}

char* day14p2(const char* input) {
    t_line tmp[100];
    size_t nline = parse_all(input, tmp);

    size_t id_size = nline + 1;
    char** identifiers = find_identifiers(tmp, id_size);

    int** cost_map = build_cost_map(tmp, identifiers, id_size);

    bigint* bank = calloc(id_size, sizeof(*bank));

    // Binary search the highest fuel consuming less than 1T ore
    bigint low = 0, high = 1000000000000ll;
    while (high - low > 1) {
        memset(bank, 0, id_size * sizeof(*bank));
        bigint test = (high - low) / 2 + low;
        bigint total = buy(cost_map, bank, id_size, 0, test);

        if (total > 1000000000000ll) {
            high = test;
        } else if (total < 1000000000000ll) {
            low = test;
        } else {
            low = test;
            break;
        }
    }

    free_pp((void**)identifiers, id_size);
    free_pp((void**)cost_map, id_size);
    free(bank);

    return aoc_bigint_to_str(low);
}
