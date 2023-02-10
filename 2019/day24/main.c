#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

#define DEPTH_MAX (200 + 1)
#define MAPS_SIZE (DEPTH_MAX * 2 + 1)

typedef enum {
    UP,
    RIGHT,
    DOWN,
    LEFT
} t_edge;

static void parse_input(const char *input, bool map[5][5]) {
    for (size_t y = 0, x = 0; *input; input++) {
        if (*input == '.')
            map[y][x++] = false;
        else if (*input == '#')
            map[y][x++] = true;
        else if (*input == '\n')
            y++, x = 0;
    }
}

static int adjacent_bugs(bool map[5][5], int x, int y) {
    int total = 0;

    if (y > 0)
        total += map[y - 1][x];
    if (y < 4)
        total += map[y + 1][x];
    if (x > 0)
        total += map[y][x - 1];
    if (x < 4)
        total += map[y][x + 1];
    return total;
}

static void next_bugs(bool map[5][5]) {
    bool copy[5][5];
    memcpy(copy, map, sizeof(copy));

    for (int y = 0; y < 5; y++) {
        for (int x = 0; x < 5; x++) {
            int neighbors = adjacent_bugs(copy, x, y);
            map[y][x] = copy[y][x] ? neighbors == 1 : (neighbors == 1 || neighbors == 2);
        }
    }
}

static unsigned int pack(bool map[5][5]) {
    bool *flat = (bool *)map;
    unsigned int total = 0;
    for (size_t i = 0; i < 25; i++)
        total |= flat[i] << i;
    return total;
}

static int bugs_in_edge(const bool map[5][5], t_edge edge) {
    int total = 0;

    for (int i = 0; i < 5; i++) {
        switch (edge) {
            case UP:
                total += map[0][i];
                break;
            case DOWN:
                total += map[4][i];
                break;
            case LEFT:
                total += map[i][0];
                break;
            case RIGHT:
                total += map[i][4];
                break;
        }
    }
    return total;
}

static int recursive_neighbors(const bool (*maps)[5][5], int index, int x, int y) {
    int total = 0;

    // center tiles
    if (x == 2 && y == 1)  // 8
        total += bugs_in_edge(maps[index + 1], UP);
    else if (x == 2 && y == 3)  // 18
        total += bugs_in_edge(maps[index + 1], DOWN);
    else if (x == 1 && y == 2)  // 12
        total += bugs_in_edge(maps[index + 1], LEFT);
    else if (x == 3 && y == 2)  // 14
        total += bugs_in_edge(maps[index + 1], RIGHT);

    if (y == 0)  // up
        total += maps[index - 1][1][2];
    else
        total += maps[index][y - 1][x];

    if (y == 4)  // down
        total += maps[index - 1][3][2];
    else
        total += maps[index][y + 1][x];

    if (x == 0)  // left
        total += maps[index - 1][2][1];
    else
        total += maps[index][y][x - 1];

    if (x == 4)  // right
        total += maps[index - 1][2][3];
    else
        total += maps[index][y][x + 1];

    return total;
}

static void update_single_map(const bool (*maps)[5][5], bool (*into)[5][5], int index) {
    for (int y = 0; y < 5; y++) {
        for (int x = 0; x < 5; x++) {
            // center stays at 0 and is where the recursion happens
            if (!(x == 2 && y == 2)) {
                int neighbors = recursive_neighbors(maps, index, x, y);
                into[index][y][x] = maps[index][y][x] ? (neighbors == 1) : (neighbors == 1 || neighbors == 2);
            }
        }
    }
}

static void simulate_recursive_maps(const bool (*maps)[5][5], bool (*next)[5][5]) {
    // Edges are always empty for ease of use
    for (int i = 1; i < (MAPS_SIZE - 1); i++)
        update_single_map(maps, next, i);
}

static int count_bugs(bool (*maps)[5][5]) {
    bool *flat = (bool *)maps;
    int total = 0;

    for (int i = 0; i < (MAPS_SIZE * 25); i++)
        total += flat[i];
    return total;
}

char *day24p1(const char *input) {
    bool map[5][5];
    bool *cache = calloc((1 << 25), sizeof(*cache));

    parse_input(input, map);
    int rating = pack(map);

    while (!cache[rating]) {
        cache[rating] = true;
        next_bugs(map);
        rating = pack(map);
    }

    free(cache);
    return aoc_asprintf("%d", rating);
}

char *day24p2(const char *input) {
    bool(*maps)[5][5];
    bool(*copy)[5][5];
    maps = calloc(MAPS_SIZE, sizeof(*maps));
    copy = calloc(MAPS_SIZE, sizeof(*copy));

    parse_input(input, maps[DEPTH_MAX]);

    for (int times = 0; times < 200; times++) {
        simulate_recursive_maps(maps, copy);

        // Swap the pointers so we can reuse the memory and avoid a copy
        bool(*tmp)[5][5] = copy;
        copy = maps;
        maps = tmp;
    }

    int total = count_bugs(maps);

    free(copy);
    free(maps);
    return aoc_asprintf("%d", total);
}
