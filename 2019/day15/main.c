#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"

// Define this to enable output and step by step solution
#undef AOC_15_INTERACTIVE

#define MAP_SIZE 42

typedef enum {
    DIR_NORTH = 1,
    DIR_SOUTH,
    DIR_WEST,
    DIR_EAST
} t_dir;

typedef enum {
    STATUS_WALL,
    STATUS_OK,
    STATUS_OXYGEN
} t_droid_status;

typedef enum {
    TILE_UNKNOWN,
    TILE_EMPTY,
    TILE_WALL,
    TILE_OXYGEN
} t_tile;

static t_intcode_result run_intcode(const char* input) {
    t_intcode_state state = aoc_intcode_boot(input);
    return aoc_intcode_eval(state);
}

static t_point2d offset_for_dir(t_dir dir) {
    switch (dir) {
        case DIR_NORTH:
            return point2d_make(0, -1);
        case DIR_SOUTH:
            return point2d_make(0, 1);
        case DIR_EAST:
            return point2d_make(1, 0);
        case DIR_WEST:
            return point2d_make(-1, 0);
    }
    return POINT2D_ZERO;
}

static t_dir pick_direction(t_tile map[MAP_SIZE][MAP_SIZE], t_point2d droid, t_point2d last_pos) {
    int start_dir = aoc_random_uniform(0, 4);

    //  Try visiting unknowns
    for (int i = 0; i < 4; i++) {
        t_dir dir = (start_dir + i) % 4 + 1;
        t_point2d target = point2d_add(droid, offset_for_dir(dir));
        if (map[target.y][target.x] == TILE_UNKNOWN) {
            return dir;
        }
    }

    // Try going into a different direction
    for (int i = 0; i < 4; i++) {
        t_dir dir = (start_dir + i) % 4 + 1;
        t_point2d target = point2d_add(droid, offset_for_dir(dir));
        if ((target.x != last_pos.x || target.y != last_pos.y) && map[target.y][target.x] == TILE_EMPTY) {
            return dir;
        }
    }

    // Pick any valid direction
    for (int i = 0; i < 4; i++) {
        t_dir dir = (start_dir + i) % 4 + 1;
        t_point2d target = point2d_add(droid, offset_for_dir(dir));
        if (map[target.y][target.x] != TILE_WALL) {
            return dir;
        }
    }

    return 0;
}

#ifdef AOC_15_INTERACTIVE
static char tile_to_c(t_tile tile) {
    return " .#$"[tile];
}

static void print_map(t_tile map[MAP_SIZE][MAP_SIZE], t_point2d droid, t_point2d start) {
    for (int y = 0; y < MAP_SIZE; y++) {
        for (int x = 0; x < MAP_SIZE; x++) {
            if (droid.x == x && droid.y == y)
                putchar('D');
            else if (start.x == x && start.y == y)
                putchar('@');
            else
                putchar(tile_to_c(map[y][x]));
        }
        putchar('\n');
    }
}
#endif

static void map_area(t_tile map[MAP_SIZE][MAP_SIZE], t_point2d droid, t_intcode_result result) {
#ifdef AOC_15_INTERACTIVE
    t_point2d start = droid;
#endif
    t_point2d last_pos = droid;
    intcode_set_input1(&result.state, 0);

    // Run an arbitrary number of steps to map the whole area
    for (int steps = 0; steps < 200000; steps++) {
        t_dir dir = pick_direction(map, droid, last_pos);
        t_point2d target_pos = point2d_add(droid, offset_for_dir(dir));

        result.state.input.data[0] = dir;
        result.state.input.head = 0;
        result = aoc_intcode_eval(result.state);

        last_pos = droid;
        if (result.state.output.head > 0) {
            t_droid_status status = result.state.output.data[0];
            switch (status) {
                case STATUS_WALL:
                    map[target_pos.y][target_pos.x] = TILE_WALL;
                    break;
                case STATUS_OK:
                    map[target_pos.y][target_pos.x] = TILE_EMPTY;
                    droid = target_pos;
                    break;
                case STATUS_OXYGEN:
                    map[target_pos.y][target_pos.x] = TILE_OXYGEN;
                    droid = target_pos;
                    break;
            }
            result.state.output.head = 0;
        }

#ifdef AOC_15_INTERACTIVE
        print_map(map, droid, start);
        getchar();
#endif
    }
}

static t_point2d find_tile(t_tile map[MAP_SIZE][MAP_SIZE], t_tile target) {
    for (int y = 0; y < MAP_SIZE; y++) {
        for (int x = 0; x < MAP_SIZE; x++) {
            if (map[y][x] == target)
                return point2d_make(x, y);
        }
    }
    return POINT2D_ZERO;
}

static int bfs(t_tile map[MAP_SIZE][MAP_SIZE], t_point2d start, t_point2d end) {
    t_point2d stack[MAP_SIZE * MAP_SIZE];
    t_point2d next[MAP_SIZE * MAP_SIZE];
    bool visited[MAP_SIZE][MAP_SIZE] = {0};
    int stack_sz = 0;
    int next_sz = 0;

    stack[stack_sz++] = start;
    int steps;
    for (steps = 0; stack_sz > 0; steps++) {
        for (int i = 0; i < stack_sz; i++) {
            if (point2d_eq(stack[i], end)) {
                return steps;
            }

            for (t_dir dir = DIR_NORTH; dir <= DIR_EAST; dir++) {
                t_point2d target = point2d_add(stack[i], offset_for_dir(dir));

                if (target.x >= 0 && target.x < MAP_SIZE
                    && target.y >= 0 && target.y < MAP_SIZE
                    && !visited[target.y][target.x]
                    && map[target.y][target.x] != TILE_WALL) {
                    visited[target.y][target.x] = true;
                    next[next_sz++] = target;
                }
            }
        }

        memcpy(stack, next, sizeof(*next) * next_sz);
        stack_sz = next_sz;
        next_sz = 0;
    }

    return steps - 1;
}

char* day15p1(const char* input) {
    t_intcode_result result = run_intcode(input);
    t_tile map[MAP_SIZE][MAP_SIZE] = {0};
    t_point2d droid = point2d_make(MAP_SIZE / 2, MAP_SIZE / 2);

    map_area(map, droid, result);
    intcode_free_result(&result);

    t_point2d end = find_tile(map, TILE_OXYGEN);
    int res = bfs(map, droid, end);

    return aoc_asprintf("%d", res);
}

char* day15p2(const char* input) {
    t_intcode_result result = run_intcode(input);
    t_tile map[MAP_SIZE][MAP_SIZE] = {0};
    t_point2d droid = point2d_make(MAP_SIZE / 2, MAP_SIZE / 2);

    map_area(map, droid, result);
    intcode_free_result(&result);

    int res = bfs(map, find_tile(map, TILE_OXYGEN), point2d_make(-1, -1));

    return aoc_asprintf("%d", res);
}
