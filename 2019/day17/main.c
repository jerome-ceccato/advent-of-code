#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

typedef enum {
    TILE_SCAFFOLD = '#',
    TILE_EMPTY = '.'
} t_tile;

typedef enum {
    NORTH,
    EAST,
    SOUTH,
    WEST
} t_dir;

typedef struct {
    t_point2d pos;
    t_dir dir;
} t_robot;

#define TRACE_SZ 1000

static const char* const robot_characters = "^>v<";

static t_intcode_result run_intcode(const char* input, int mode) {
    t_intcode_state state = aoc_intcode_boot(input);
    state.memory.data[0] = mode;
    aoc_intcode_upgrade_memory(&state, 4096);
    return aoc_intcode_eval(state);
}

static t_tile** decode_output(t_bigint_stream* output, size_t* out_width, size_t* out_height, t_robot* out_robot) {
    size_t width = 0;
    for (width = 0; width < output->head && output->data[width] != '\n'; width++) {}
    size_t height = output->head / (width + 1);

    t_tile** map = malloc(sizeof(*map) * height);
    size_t i = 0;
    for (size_t y = 0; y < height; y++) {
        map[y] = malloc(sizeof(**map) * width);
        for (size_t x = 0; x < width; x++) {
            char* p = strchr(robot_characters, output->data[i]);
            if (p) {
                out_robot->pos = point2d_make(x, y);
                out_robot->dir = p - robot_characters;
                map[y][x] = TILE_SCAFFOLD;
                i++;
            } else {
                map[y][x] = output->data[i++];
            }
        }
        i++;
    }

    *out_width = width;
    *out_height = height;
    return map;
}

#ifdef DAY17_PRINT
static void print_map(t_tile** map, size_t width, size_t height, t_robot robot) {
    for (size_t y = 0; y < height; y++) {
        for (size_t x = 0; x < width; x++) {
            if (robot.pos.x == (int)x && robot.pos.y == (int)y) {
                putchar(robot_characters[robot.dir]);
            } else {
                putchar(map[y][x]);
            }
        }
        putchar('\n');
    }
}
#endif

static void free_map(t_tile** map, size_t height) {
    for (size_t y = 0; y < height; y++) {
        free(map[y]);
    }
    free(map);
}

static t_tile map_safe_get(t_tile** map, size_t width, size_t height, t_point2d pos) {
    if (pos.x >= 0 && (size_t)pos.x < width && pos.y >= 0 && (size_t)pos.y < height)
        return map[pos.y][pos.x];
    return TILE_EMPTY;
}

static bool is_intersection(t_tile** map, size_t width, size_t height, t_point2d pos) {
    return map_safe_get(map, width, height, pos) == TILE_SCAFFOLD
           && map_safe_get(map, width, height, point2d_make(pos.x, pos.y + 1)) == TILE_SCAFFOLD
           && map_safe_get(map, width, height, point2d_make(pos.x, pos.y - 1)) == TILE_SCAFFOLD
           && map_safe_get(map, width, height, point2d_make(pos.x + 1, pos.y)) == TILE_SCAFFOLD
           && map_safe_get(map, width, height, point2d_make(pos.x - 1, pos.y)) == TILE_SCAFFOLD;
}

static int calibrate(t_tile** map, size_t width, size_t height) {
    int total = 0;
    for (size_t y = 1; y < height - 1; y++) {
        for (size_t x = 1; x < width - 1; x++) {
            if (is_intersection(map, width, height, point2d_make(x, y)))
                total += x * y;
        }
    }
    return total;
}

static t_point2d move(t_point2d pos, t_dir dir) {
    switch (dir) {
        case NORTH:
            return point2d_make(pos.x, pos.y - 1);
        case SOUTH:
            return point2d_make(pos.x, pos.y + 1);
        case EAST:
            return point2d_make(pos.x + 1, pos.y);
        case WEST:
            return point2d_make(pos.x - 1, pos.y);
    }
    return pos;
}

static bool was_visited(t_point2d target, t_point2d* visited, size_t visited_s) {
    for (size_t i = 0; i < visited_s; i++) {
        if (point2d_eq(target, visited[i]))
            return true;
    }
    return false;
}

// Transforms a sequence like "AAAA,BBBBBBB,CC,AAAA" into "A,B,C,A"
static char* collapse_sequence(const char* seq) {
    size_t size = 1;
    for (int i = 0; seq[i]; i++)
        if (seq[i] == ',')
            size++;

    char* ret = malloc(size + 1);
    int reti = 0;
    ret[size] = 0;
    for (int i = 0; seq[i]; i++) {
        if (seq[i] == ',')
            ret[reti++] = ',';
        else {
            char c = seq[i];
            ret[reti++] = c;
            while (seq[i + 1] == c)
                i++;
        }
    }

    return ret;
}

// Replaces all occurences of a pattern by a repeated character
static void replace_all(char* sequence, const char* pattern, size_t pattern_size, char replacement) {
    char* p;
    while ((p = strstr(sequence, pattern))) {
        for (size_t i = 0; i < pattern_size; i++)
            *(p++) = replacement;
    }
}

// Checks if the sequence can be split in 3 parts with the given start and end parts
static char* try_replace_sequence(char* sequence, char* start, char* end) {
    int startlen = strlen(start);
    int endlen = strlen(end);

    replace_all(sequence, start, startlen, 'A');
    replace_all(sequence, end, endlen, 'C');

    char b[1000] = {0};
    int i = 0, bi = 0;

    while (sequence[i]) {
        while (sequence[i] && (sequence[i] == 'A' || sequence[i] == 'C' || sequence[i] == ','))
            i++;

        if (!sequence[i])
            break;

        // Record the first new sequence
        if (!bi) {
            while (sequence[i] && !(sequence[i] == 'A' || sequence[i] == 'C'))
                b[bi++] = sequence[i++];
            b[--bi] = 0;

            // Sequences cannot be longer than 20 characters
            if (bi > 20)
                return NULL;
        } else {
            // Match with the existing sequence
            if (!strncmp(sequence + i, b, bi))
                i += bi;
            else
                return NULL;
        }
    }

    replace_all(sequence, b, bi, 'B');
    char* main_seq = collapse_sequence(sequence);
    char* result = aoc_asprintf("%s\n%s\n%s\n%s\nn\n", main_seq, start, b, end);
    free(main_seq);
    return result;
}

static char* try_path(const char* sequence) {
    char start[22] = {0};
    char end[22] = {0};

    // The sequence must start with A and end with C, try all possible A/C <= 20 char, then see if a single B is left
    size_t size = strlen(sequence);
    for (size_t starti = 3; starti <= 20; starti++) {
        strncpy(start, sequence, starti);
        start[starti] = 0;
        for (size_t endi = 3; endi <= 20; endi++) {
            if (sequence[starti] == ',' && sequence[size - endi - 1] == ',') {
                strncpy(end, sequence + (size - endi), endi);
                end[endi] = 0;

                char* seq_dup = strdup(sequence);
                char* result = try_replace_sequence(seq_dup, start, end);
                free(seq_dup);

                if (result)
                    return result;
            }
        }
    }
    return NULL;
}

// Explore all possible paths (turning at every intersections) until a solution is found
static char* explore(t_tile** map, size_t width, size_t height, t_robot robot, t_point2d* visited, size_t visited_s, char* trace, int scaffolds) {
    int n = 0;
    int tracei = strlen(trace);

    while (true) {
        t_point2d next = move(robot.pos, robot.dir);
        bool next_is_visited = was_visited(next, visited, visited_s);
        bool next_is_intersection = is_intersection(map, width, height, next);

        // if we've already visited a non-intersection, we're going backwards, stop
        if (next_is_visited && !next_is_intersection) {
            return NULL;
        }

        // if ahead is a scaffold
        if (map_safe_get(map, width, height, next) == TILE_SCAFFOLD) {
            robot.pos = next;

            if (!next_is_visited)
                visited[visited_s++] = next;
            n++;

            // recurse to try to turn left or right too
            if (next_is_intersection) {
                for (t_dir dir = NORTH; dir <= WEST; dir++) {
                    bool is_right = dir == (robot.dir + 1) % 4;
                    bool is_left = dir == (robot.dir + 3) % 4;
                    if (is_right || is_left) {
                        t_point2d expected = move(robot.pos, dir);
                        if (!was_visited(expected, visited, visited_s)) {
                            char* next_trace = calloc(TRACE_SZ, 1);
                            int traceinext = tracei;
                            memcpy(next_trace, trace, tracei);

                            if (n)
                                traceinext += sprintf(next_trace + traceinext, "%d,", n);
                            if (is_right)
                                next_trace[traceinext++] = 'R';
                            else
                                next_trace[traceinext++] = 'L';
                            next_trace[traceinext++] = ',';

                            t_point2d* next_visited = malloc(sizeof(t_point2d) * 1000);
                            memcpy(next_visited, visited, visited_s * sizeof(*visited));

                            char* res = explore(map, width, height, (t_robot){robot.pos, dir}, next_visited, visited_s, next_trace, scaffolds);
                            if (res)
                                return res;

                            free(next_trace);
                            free(next_visited);
                        }
                    }
                }
            }
        } else {
            // if ahead is empty or oob, try turning
            bool found = false;
            for (t_dir dir = NORTH; !found && dir <= WEST; dir++) {
                next = move(robot.pos, dir);
                if (map_safe_get(map, width, height, next) == TILE_SCAFFOLD && !was_visited(next, visited, visited_s)) {
                    if (n)
                        tracei += sprintf(trace + tracei, "%d,", n);
                    if (dir == (robot.dir + 1) % 4)
                        trace[tracei++] = 'R';
                    else
                        trace[tracei++] = 'L';
                    trace[tracei++] = ',';

                    n = 1;
                    robot.pos = next;
                    robot.dir = dir;
                    visited[visited_s++] = next;
                    found = true;
                }
            }

            if (!found) {
                if (visited_s >= (size_t)(scaffolds - 1)) {
                    sprintf(trace + tracei, "%d", n);
                    // printf("%s\n", trace);
                    return try_path(trace);
                }
                return NULL;
            }
        }
    }
}

static void encode_input_procedure(t_intcode_state* state, const char* full_input) {
    state->input.head = 0;
    state->input.size = strlen(full_input);
    state->input.data = malloc(sizeof(*state->input.data) * state->input.size);
    for (size_t i = 0; i < state->input.size; i++)
        state->input.data[i] = full_input[i];
}

char* day17p1(const char* input) {
    t_intcode_result result = run_intcode(input, 1);
    t_tile** map;
    size_t width, height;
    t_robot robot;

    map = decode_output(&result.state.output, &width, &height, &robot);
    int alignment = calibrate(map, width, height);

    free_map(map, height);
    intcode_free_result(&result);
    return aoc_asprintf("%d", alignment);
}

char* day17p2(const char* input) {
    t_intcode_result result = run_intcode(input, 2);
    t_tile** map;
    size_t width, height;
    t_robot robot;

    map = decode_output(&result.state.output, &width, &height, &robot);
#ifdef DAY17_PRINT
    print_map(map, width, height, robot);
#endif

    int scaffolds = 0;
    for (size_t y = 0; y < height; y++)
        for (size_t x = 0; x < width; x++)
            if (map[y][x] == TILE_SCAFFOLD)
                scaffolds++;

    char* sequence = explore(map, width, height, robot, malloc(sizeof(t_point2d) * 1000), 0, calloc(TRACE_SZ, 1), scaffolds);
    result.state.output.head = 0;
    encode_input_procedure(&result.state, sequence);

    result = aoc_intcode_eval(result.state);
    bigint dust_collected = intcode_last_output(&result);

    free(sequence);
    free_map(map, height);
    intcode_free_result(&result);
    return aoc_bigint_to_str(dust_collected);
}
