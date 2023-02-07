#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
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

// Record the position of all keys and doors so we can look them up in constant time
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
                default:
                    break;
            }
    return ret;
}

// Stores the best time for a given set of keys left (encoded)
typedef struct {
    int keys_left;
    int best;
} t_memo_path;

// Memoize the sub-times per path
typedef struct {
    // This serves as a makeshift hashtable to encode start position + owned keys => shortest path
    // Since there are (1<<26) possible key combinations, we use a shorter array and traverse it instead of
    // having constant access time
    t_memo_path*** paths;  // [height][width][MEMO_PATH_COUNT]
} t_memo;

struct s_stats {
    clock_t start;
    bigint loops;
    bigint real_loops;
    bigint memo_hit;
    bigint memo_size;
};
static struct s_stats g_stats;

void print_stats() {
    clock_t end = clock();
    double elapsed = (double)(end - g_stats.start) / CLOCKS_PER_SEC;

    printf("=== Stats =======\n");
    printf("Time: %.6f\n", elapsed);
    printf("Loops: %s\n", aoc_bigint_to_str(g_stats.loops));
    printf("Real loops: %s\n", aoc_bigint_to_str(g_stats.real_loops));
    printf("Memo hits: %s\n", aoc_bigint_to_str(g_stats.memo_hit));
    printf("Memo record size: %s\n", aoc_bigint_to_str(g_stats.memo_size));
    printf("=================\n");
}

#define MEMO_PATH_COUNT 10000

static int encode_keys(char** map, t_metadata metadata) {
    int key = 0;
    for (int i = 0; i < 26; i++) {
        if (metadata.keys[i].x != -1 && map[metadata.keys[i].y][metadata.keys[i].x] == 'a' + i)
            key |= (1 << i);
    }
    return key;
}

static int bfs(char** map, size_t width, size_t height, t_point2d start, t_metadata metadata, t_memo* memo) {
    t_point2d* stack;
    t_point2d* next;
    bool** visited;
    int stack_sz = 0;
    int next_sz = 0;
    int order[26] = {0};
    int orderi = 0;
    int reachable_keys[26] = {0};
    int run_key = encode_keys(map, metadata);

    g_stats.loops++;

    if (!metadata.keys_left)
        return 0;

    for (int ek = 0; ek < MEMO_PATH_COUNT; ek++) {
        if (memo->paths[start.y][start.x][ek].keys_left == run_key) {
            g_stats.memo_hit++;
            return memo->paths[start.y][start.x][ek].best;
        }
    }

    g_stats.real_loops++;

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

            int candidate = bfs(map, width, height, metadata.keys[i], metadata, memo);
            if (candidate != INT_MAX) {
                candidate += reachable_keys[i];
                local_best = min(local_best, candidate);
            }

            map[metadata.keys[i].y][metadata.keys[i].x] = 'a' + i;
            if (metadata.doors[i].x != -1)
                map[metadata.doors[i].y][metadata.doors[i].x] = 'A' + i;
            metadata.keys_left++;
        }
    }

    bool done = false;
    for (int ek = 0; ek < MEMO_PATH_COUNT; ek++) {
        if (memo->paths[start.y][start.x][ek].keys_left == run_key) {
            memo->paths[start.y][start.x][ek].best = local_best;
            done = true;
            break;
        }
        if (memo->paths[start.y][start.x][ek].best == 0) {
            memo->paths[start.y][start.x][ek].keys_left = run_key;
            memo->paths[start.y][start.x][ek].best = local_best;
            g_stats.memo_size++;
            done = true;
            break;
        }
    }
    if (!done)
        printf("Memo too small for %d,%d\n", start.x, start.y);
    return local_best;
}

char* day18p1(const char* input) {
    size_t width, height;
    t_point2d start;

    return NULL;
    char** map = parse_map(input, &width, &height, &start);
    t_metadata metadata = build_metadata(map, width, height);
    t_memo memo;
    memo.paths = malloc(sizeof(*memo.paths) * height);
    for (size_t i = 0; i < height; i++) {
        memo.paths[i] = malloc(sizeof(**memo.paths) * width);
        for (size_t j = 0; j < width; j++) {
            memo.paths[i][j] = calloc(MEMO_PATH_COUNT, sizeof(***memo.paths));
        }
    }

    memset(&g_stats, 0, sizeof(g_stats));
    g_stats.start = clock();
    int fastest = bfs(map, width, height, start, metadata, &memo);

    print_stats();

    free_pp((void**)map, height);
    free_ppp((void***)memo.paths, height, width);
    return aoc_asprintf("%d", fastest);
}

static void split_entrance(char** map, t_point2d start, t_point2d robots[4]) {
    robots[0] = point2d_add(start, point2d_make(-1, -1));
    robots[1] = point2d_add(start, point2d_make(1, -1));
    robots[2] = point2d_add(start, point2d_make(-1, 1));
    robots[3] = point2d_add(start, point2d_make(1, 1));

    map[start.y][start.x] = '#';
    map[start.y - 1][start.x] = '#';
    map[start.y + 1][start.x] = '#';
    map[start.y][start.x - 1] = '#';
    map[start.y][start.x + 1] = '#';
}

typedef struct {
    int keys_left;
    t_point2d robots[4];
    int best;
} t_memo2_path;

// Memoize the sub-times per path
typedef struct {
    // Because we now have 4 points, it's impossible to use all their indices as keys (too large)
    // Instead, we calculate some number from the coordinates to reduce the amount of collisions, but still need to check
    // in the memo.
    t_memo2_path** paths;  // [robot_pos_id][MEMO_PATH_COUNT]
} t_memo2;

static int encode_robots(t_point2d robots[4]) {
    int total = 0;
    for (int i = 0; i < 4; i++)
        total += (1 << i) * (robots[i].x + robots[i].y);
    return total;
}

static bool robot_eq(t_point2d robots[4], t_point2d other[4]) {
    for (int i = 0; i < 4; i++)
        if (!point2d_eq(robots[i], other[i]))
            return false;
    return true;
}

static int bfsp2(char** map, size_t width, size_t height, t_point2d robots[4], t_metadata metadata, t_memo2* memo) {
    t_point2d* stack;
    t_point2d* next;
    bool** visited;
    int order[4][26] = {0};
    int orderi[4] = {0};
    int reachable_keys[4][26] = {0};
    int run_key = encode_keys(map, metadata);
    int robot_key = encode_robots(robots);

    g_stats.loops++;

    if (!(g_stats.loops % 100000))
        print_stats();

    if (!metadata.keys_left)
        return 0;

    for (int ek = 0; ek < MEMO_PATH_COUNT; ek++) {
        if (memo->paths[robot_key][ek].keys_left == run_key && robot_eq(memo->paths[robot_key][ek].robots, robots)) {
            g_stats.memo_hit++;
            return memo->paths[robot_key][ek].best;
        }
    }

    g_stats.real_loops++;

    // printf("key; %x\n", run_key);
    //  print_map(map, width, height, start);

    stack = malloc(sizeof(*stack) * (width * height));
    next = malloc(sizeof(*next) * (width * height));
    visited = malloc(sizeof(*visited) * height);
    for (size_t i = 0; i < height; i++)
        visited[i] = calloc(width, sizeof(**visited));

    for (int roboti = 0; roboti < 4; roboti++) {
        int stack_sz = 0;
        int next_sz = 0;
        // printf("- %d\n", roboti);
        for (size_t i = 0; i < height; i++)
            memset(visited[i], 0, width * sizeof(**visited));

        // printf("memset\n");
        stack[stack_sz++] = robots[roboti];
        // printf("%d,%d\n", stack[0].x, stack[0].y);
        int steps;
        for (steps = 0; stack_sz > 0; steps++) {
            // printf("] %d\n", steps);
            for (int i = 0; i < stack_sz; i++) {
                // printf("(%d,%d)\n", stack[i].x, stack[i].y);
                char stackc = map[stack[i].y][stack[i].x];
                // printf("stack %c\n", stackc);
                if (tile_type(stackc) == TYPE_KEY) {
                    reachable_keys[roboti][stackc - 'a'] = steps;
                    order[roboti][orderi[roboti]++] = stackc - 'a';
                }
                // printf("key\n");
                for (t_dir dir = DIR_NORTH; dir <= DIR_WEST; dir++) {
                    t_point2d target = point2d_add(stack[i], offset_for_dir(dir));
                    // printf("target %d,%d\n", target.x, target.y);
                    // printf("map %c\n", map[target.y][target.x]);
                    if (target.x >= 0 && (size_t)target.x < width
                        && target.y >= 0 && (size_t)target.y < height
                        && !visited[target.y][target.x]
                        && walkable(tile_type(map[target.y][target.x]))) {
                        // printf("walkable\n");
                        visited[target.y][target.x] = true;
                        next[next_sz++] = target;
                    }
                }
                // printf("dir\n");
            }

            memcpy(stack, next, sizeof(*next) * next_sz);
            stack_sz = next_sz;
            next_sz = 0;
        }
    }

    free(stack);
    free(next);
    free_pp((void**)visited, height);

    int local_best = INT_MAX;

    for (int roboti = 0; roboti < 4; roboti++) {
        for (int j = 0; j < orderi[roboti]; j++) {
            int i = order[roboti][j];
            if (reachable_keys[roboti][i] > 0) {
                map[metadata.keys[i].y][metadata.keys[i].x] = '.';
                if (metadata.doors[i].x != -1)
                    map[metadata.doors[i].y][metadata.doors[i].x] = '.';
                metadata.keys_left--;

                t_point2d next_robots[4];
                memcpy(next_robots, robots, sizeof(next_robots));

                next_robots[roboti] = metadata.keys[i];
                int candidate = bfsp2(map, width, height, next_robots, metadata, memo);
                if (candidate != INT_MAX) {
                    candidate += reachable_keys[roboti][i];
                    local_best = min(local_best, candidate);
                }

                map[metadata.keys[i].y][metadata.keys[i].x] = 'a' + i;
                if (metadata.doors[i].x != -1)
                    map[metadata.doors[i].y][metadata.doors[i].x] = 'A' + i;
                metadata.keys_left++;
            }
        }
    }

    bool done = false;
    for (int ek = 0; ek < MEMO_PATH_COUNT; ek++) {
        if (memo->paths[robot_key][ek].keys_left == run_key && robot_eq(memo->paths[robot_key][ek].robots, robots)) {
            memo->paths[robot_key][ek].best = local_best;
            done = true;
            break;
        }
        if (memo->paths[robot_key][ek].best == 0) {
            memo->paths[robot_key][ek].keys_left = run_key;
            memo->paths[robot_key][ek].best = local_best;
            memcpy(memo->paths[robot_key][ek].robots, robots, sizeof(*robots) * 4);
            g_stats.memo_size++;
            done = true;
            break;
        }
    }
    if (!done)
        printf("Memo too small\n");
    return local_best;
}

char* day18p2(const char* input) {
    size_t width, height;
    t_point2d start;
    t_point2d robots[4];

    char** map = parse_map(input, &width, &height, &start);
    t_metadata metadata = build_metadata(map, width, height);
    split_entrance(map, start, robots);

    t_memo2 memo;
    printf("Let's alloc a lot of memory\n");
    size_t memo_sz = (16 * width * height);
    memo.paths = malloc(sizeof(*memo.paths) * memo_sz);
    for (size_t i = 0; i < memo_sz; i++)
        memo.paths[i] = calloc(MEMO_PATH_COUNT, sizeof(**memo.paths));

    // print_map(map, width, height, point2d_make(-1, -1));
    memset(&g_stats, 0, sizeof(g_stats));
    g_stats.start = clock();
    printf("Let's goooo\n");
    int fastest = bfsp2(map, width, height, robots, metadata, &memo);

    print_stats();

    free_pp((void**)map, height);
    free_pp((void**)memo.paths, memo_sz);
    return aoc_asprintf("%d", fastest);
}

/*
With fixed memo path (mem 10000)
=== Stats =======
Time: 34.883000
Loops: 673731
Real loops: 124142
Memo best hits: 266051
Memo path hits: 283516
Memo record size: 46729
=================

With local count (no acc) (mem 10000)
=== Stats =======
Time: 18.566000
Loops: 331031
Real loops: 61847
Memo best hits: 0
Memo path hits: 269093
Memo record size: 61847
=================

p2 stats
=== Stats =======
Time: 150.116000
Loops: 2033565
Real loops: 427122
Memo hits: 1604575
Memo record size: 418031
=================
*/
