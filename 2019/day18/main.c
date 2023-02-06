#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"

typedef enum {
    DIR_NORTH,
    DIR_EAST,
    DIR_SOUTH,
    DIR_WEST
} t_dir;

typedef enum {
    TYPE_WALL,
    TYPE_EMPTY,
    TYPE_KEY,
    TYPE_DOOR
} t_tile_type;

typedef struct {
    t_point2d keys[26];
    t_point2d doors[26];

    int keys_left;
} t_metadata;

static char** parse_map(const char* input, size_t* out_width, size_t* out_height, t_point2d* out_start) {
    size_t width = strchr(input, '\n') - input;
    size_t height = strlen(input) / width;

    char** map = malloc(sizeof(*map) * height);
    size_t i = 0;
    for (size_t y = 0; y < height; y++) {
        map[y] = malloc(sizeof(**map) * width);
        for (size_t x = 0; x < width; x++) {
            if (input[i] == '@') {
                map[y][x] = '.';
                out_start->x = x;
                out_start->y = y;
            } else {
                map[y][x] = input[i];
            }
            i++;
        }
        i++;
    }

    *out_width = width;
    *out_height = height;
    return map;
}

static void print_map(char** map, size_t width, size_t height, t_point2d start) {
    for (size_t y = 0; y < height; y++) {
        for (size_t x = 0; x < width; x++) {
            if (start.x == (int)x && start.y == (int)y) {
                putchar('@');
            } else {
                putchar(map[y][x]);
            }
        }
        putchar('\n');
    }
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

static t_tile_type tile_type(char c) {
    if (c >= 'a' && c <= 'z') {
        return TYPE_KEY;
    } else if (c >= 'A' && c <= 'Z') {
        return TYPE_DOOR;
    } else if (c == '#') {
        return TYPE_WALL;
    } else {
        return TYPE_EMPTY;
    }
}

static bool walkable(t_tile_type type) {
    return type == TYPE_EMPTY || type == TYPE_KEY;
}

static t_metadata build_metadata(char** map, size_t width, size_t height) {
    t_metadata ret;

    for (int i = 0; i < 26; i++) {
        ret.keys[i] = point2d_make(-1, -1);
        ret.doors[i] = point2d_make(-1, -1);
    }
    ret.keys_left = 0;

    for (size_t y = 0; y < height; y++)
        for (size_t x = 0; x < width; x++)
            switch (tile_type(map[y][x])) {
                case TYPE_KEY:
                    ret.keys[map[y][x] - 'a'] = point2d_make(x, y);
                    ret.keys_left++;
                    break;
                case TYPE_DOOR:
                    ret.doors[map[y][x] - 'A'] = point2d_make(x, y);
                    break;
            }
    return ret;
}

typedef struct {
    int keys_left;
    int steps;
    int best;
} t_memo_path;

typedef struct {
    int best;
    t_memo_path*** paths;  // [height][width][100]
} t_memo;

#define YOLO_PATH_MEM 1000

static int encode_keys(char** map, t_metadata metadata) {
    int key = 0;
    for (int i = 0; i < 26; i++) {
        if (metadata.keys[i].x != -1 && map[metadata.keys[i].y][metadata.keys[i].x] == 'a' + i)
            key |= (1 << i);
    }
    return key;
}

static int bfs(char** map, size_t width, size_t height, t_point2d start, t_metadata metadata, int acc_steps, t_memo* memo) {
    t_point2d* stack;
    t_point2d* next;
    bool** visited;
    int stack_sz = 0;
    int next_sz = 0;
    int order[26] = {0};
    int orderi = 0;
    int reachable_keys[26] = {0};
    int run_key = encode_keys(map, metadata);

    if (!metadata.keys_left) {
        if (acc_steps < memo->best) {
            memo->best = acc_steps;
            printf("-> %d\n", acc_steps);
        }

        return acc_steps;
    }

    if (acc_steps >= memo->best) {
        return INT_MAX;
    }

    for (int ek = 0; ek < YOLO_PATH_MEM; ek++) {
        if (memo->paths[start.y][start.x][ek].keys_left == run_key && acc_steps >= memo->paths[start.y][start.x][ek].steps) {
            return memo->paths[start.y][start.x][ek].best;
        }
    }

    // printf("%x\n", run_key);
    //  print_map(map, width, height, start);

    stack = malloc(sizeof(*stack) * (width * height));
    next = malloc(sizeof(*next) * (width * height));
    visited = malloc(sizeof(*visited) * height);
    for (size_t i = 0; i < height; i++)
        visited[i] = calloc(width, sizeof(**visited));

    stack[stack_sz++] = start;
    int steps;
    for (steps = 0; stack_sz > 0; steps++) {
        for (int i = 0; i < stack_sz; i++) {
            // printf("(%d,%d)\n", stack[i].x, stack[i].y);
            char stackc = map[stack[i].y][stack[i].x];
            if (tile_type(stackc) == TYPE_KEY) {
                reachable_keys[stackc - 'a'] = steps;
                order[orderi++] = stackc - 'a';
            }

            for (t_dir dir = DIR_NORTH; dir <= DIR_WEST; dir++) {
                t_point2d target = point2d_add(stack[i], offset_for_dir(dir));

                if (target.x >= 0 && (size_t)target.x < width
                    && target.y >= 0 && (size_t)target.y < height
                    && !visited[target.y][target.x]
                    && walkable(tile_type(map[target.y][target.x]))) {
                    visited[target.y][target.x] = true;
                    next[next_sz++] = target;
                }
            }
        }

        memcpy(stack, next, sizeof(*next) * next_sz);
        stack_sz = next_sz;
        next_sz = 0;
    }

    free(stack);
    free(next);
    free_pp((void**)visited, height);

    int local_best = INT_MAX;

    for (int j = 0; j < orderi; j++) {
        int i = order[j];
        if (reachable_keys[i] > 0) {
            map[metadata.keys[i].y][metadata.keys[i].x] = '.';
            if (metadata.doors[i].x != -1)
                map[metadata.doors[i].y][metadata.doors[i].x] = '.';
            metadata.keys_left--;

            int candidate = bfs(map, width, height, metadata.keys[i], metadata, reachable_keys[i] + acc_steps, memo);
            local_best = min(local_best, candidate);

            map[metadata.keys[i].y][metadata.keys[i].x] = 'a' + i;
            if (metadata.doors[i].x != -1)
                map[metadata.doors[i].y][metadata.doors[i].x] = 'A' + i;
            metadata.keys_left++;
        }
    }

    for (int ek = 0; ek < YOLO_PATH_MEM; ek++) {
        if (memo->paths[start.y][start.x][ek].best == 0) {
            memo->paths[start.y][start.x][ek].keys_left = run_key;
            memo->paths[start.y][start.x][ek].steps = acc_steps;
            memo->paths[start.y][start.x][ek].best = local_best;
            break;
        }
    }
    return local_best;
}

char* day18p1(const char* input) {
    size_t width, height;
    t_point2d start;

    char** map = parse_map(input, &width, &height, &start);
    t_metadata metadata = build_metadata(map, width, height);
    t_memo memo = (t_memo){.best = INT_MAX};
    memo.paths = malloc(sizeof(*memo.paths) * height);
    for (int i = 0; i < height; i++) {
        memo.paths[i] = malloc(sizeof(**memo.paths) * width);
        for (int j = 0; j < width; j++) {
            memo.paths[i][j] = calloc(YOLO_PATH_MEM, sizeof(***memo.paths));
        }
    }

    int fastest = bfs(map, width, height, start, metadata, 0, &memo);

    free_pp((void**)map, height);
    return aoc_asprintf("%d", fastest);
}

char* day18p2(const char* input) {
    return NULL;
}
