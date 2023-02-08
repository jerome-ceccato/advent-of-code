#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "aoc.h"

typedef struct
{
    char **map;
    int **labels;  // [height][width] map of encoded entrance labels
    size_t width;
    size_t height;

    t_point2d **lookup;  // [entrance_id][2] map of encoded entrance label to position of both portals (or just 1 if entrance or exit)
} t_board;

typedef enum {
    DIR_NORTH,
    DIR_EAST,
    DIR_SOUTH,
    DIR_WEST
} t_dir;

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
    return point2d_make(0, 0);
}

static const size_t entrance_id_size = (26 * 26) + 1;
static int encode_entrance(char first, char second) {
    return ((first - 'A') * 26) + (second - 'A') + 1;
}

static t_board parse_input(const char *input) {
    size_t full_width = strchr(input, '\n') - input;
    size_t full_height = (strlen(input) + 1) / (full_width + 1);  // account for the \n otherwise it fails when height > width...
    t_board board;

    board.width = full_width - 4;
    board.height = full_height - 4;

    board.map = malloc(sizeof(*board.map) * board.height);
    for (size_t i = 0; i < board.height; i++)
        board.map[i] = calloc(board.width, sizeof(**board.map));

    board.labels = malloc(sizeof(*board.labels) * board.height);
    for (size_t i = 0; i < board.height; i++)
        board.labels[i] = calloc(board.width, sizeof(**board.labels));

    size_t i = 0;
    for (size_t y = 0; y < full_height; y++, i++) {
        for (size_t x = 0; x < full_width; x++, i++) {
            if (y >= 2 && y < (full_height - 2) && x >= 2 && x < (full_width - 2)) {
                if (input[i] == '#' || input[i] == '.')
                    board.map[y - 2][x - 2] = input[i];
                else
                    board.map[y - 2][x - 2] = ' ';

                if (input[i] == '.') {
                    // Labels always read top to bottom or left to right
                    if (isalpha(input[i - (full_width + 1)]))  // label above
                        board.labels[y - 2][x - 2] = encode_entrance(input[i - (2 * (full_width + 1))], input[i - (full_width + 1)]);
                    else if (isalpha(input[i + (full_width + 1)]))  // label below
                        board.labels[y - 2][x - 2] = encode_entrance(input[i + (full_width + 1)], input[i + (2 * (full_width + 1))]);
                    else if (isalpha(input[i + 1]))  // label on the right
                        board.labels[y - 2][x - 2] = encode_entrance(input[i + 1], input[i + 2]);
                    else if (isalpha(input[i - 1]))  // label on the left
                        board.labels[y - 2][x - 2] = encode_entrance(input[i - 2], input[i - 1]);
                }
            }
        }
    }

    board.lookup = malloc(sizeof(*board.lookup) * entrance_id_size);
    for (size_t i = 0; i < entrance_id_size; i++) {
        board.lookup[i] = malloc(sizeof(**board.lookup) * 2);
        board.lookup[i][0] = point2d_make(-1, -1);
        board.lookup[i][1] = point2d_make(-1, -1);
    }

    for (size_t y = 0; y < board.height; y++)
        for (size_t x = 0; x < board.width; x++) {
            int label = board.labels[y][x];
            if (label) {
                if (board.lookup[label][0].x == -1)
                    board.lookup[label][0] = point2d_make(x, y);
                else
                    board.lookup[label][1] = point2d_make(x, y);
            }
        }

    return board;
}

static int bfs(t_board *board, t_point2d start, t_point2d end) {
    t_point2d *stack;
    t_point2d *next;
    bool **visited;
    int stack_sz = 0;
    int next_sz = 0;

    stack = malloc(sizeof(*stack) * (board->width * board->height));
    next = malloc(sizeof(*next) * (board->width * board->height));
    visited = malloc(sizeof(*visited) * board->height);
    for (size_t i = 0; i < board->height; i++)
        visited[i] = calloc(board->width, sizeof(**visited));

    visited[start.y][start.x] = true;
    stack[stack_sz++] = start;
    int steps;
    for (steps = 0; stack_sz > 0; steps++) {
        for (int i = 0; i < stack_sz; i++) {
            if (point2d_eq(stack[i], end)) {
                free(stack);
                free(next);
                free_pp((void **)visited, board->height);
                return steps;
            }

            for (t_dir dir = DIR_NORTH; dir <= DIR_WEST; dir++) {
                t_point2d target = point2d_add(stack[i], offset_for_dir(dir));

                if (target.x >= 0 && (size_t)target.x < board->width
                    && target.y >= 0 && (size_t)target.y < board->height
                    && !visited[target.y][target.x]
                    && board->map[target.y][target.x] == '.') {
                    visited[target.y][target.x] = true;
                    next[next_sz++] = target;
                }
            }

            int label = board->labels[stack[i].y][stack[i].x];
            if (label) {
                t_point2d target;
                if (point2d_eq(stack[i], board->lookup[label][0]))
                    target = board->lookup[label][1];
                else
                    target = board->lookup[label][0];

                if (target.x != -1) {
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
    free_pp((void **)visited, board->height);
    return -1;
}

#define MAX_STACK 10000
#define MAX_DEPTH 1000

static t_point2d flatten_point3d(t_point3d p) {
    return point2d_make(p.x, p.y);
}

static bool outer_edge(t_point2d target, t_board *board) {
    return target.x == 0
           || (size_t)target.x == (board->width - 1)
           || target.y == 0
           || (size_t)target.y == (board->height - 1);
}

static int bfs_rec(t_board *board, t_point3d start, t_point3d end) {
    t_point3d *stack;
    t_point3d *next;
    bool ***visited;
    int stack_sz = 0;
    int next_sz = 0;

    stack = malloc(sizeof(*stack) * (MAX_STACK * board->width * board->height));
    next = malloc(sizeof(*next) * (MAX_STACK * board->width * board->height));
    visited = malloc(sizeof(*visited) * MAX_DEPTH);
    for (size_t y = 0; y < MAX_DEPTH; y++) {
        visited[y] = malloc(sizeof(**visited) * board->height);
        for (size_t i = 0; i < board->height; i++)
            visited[y][i] = calloc(board->width, sizeof(***visited));
    }

    visited[start.z][start.y][start.x] = true;
    stack[stack_sz++] = start;
    int steps;
    for (steps = 0; stack_sz > 0; steps++) {
        for (int i = 0; i < stack_sz; i++) {
            if (point3d_eq(stack[i], end)) {
                free(stack);
                free(next);
                free_ppp((void ***)visited, MAX_DEPTH, board->height);
                return steps;
            }

            for (t_dir dir = DIR_NORTH; dir <= DIR_WEST; dir++) {
                t_point2d target = point2d_add(flatten_point3d(stack[i]), offset_for_dir(dir));

                if (target.x >= 0 && (size_t)target.x < board->width
                    && target.y >= 0 && (size_t)target.y < board->height
                    && !visited[stack[i].z][target.y][target.x]
                    && board->map[target.y][target.x] == '.') {
                    visited[stack[i].z][target.y][target.x] = true;
                    next[next_sz++] = point3d_make(target.x, target.y, stack[i].z);
                }
            }

            int label = board->labels[stack[i].y][stack[i].x];
            if (label && label != encode_entrance('A', 'A') && label != encode_entrance('Z', 'Z')) {
                t_point2d target;
                if (point2d_eq(flatten_point3d(stack[i]), board->lookup[label][0]))
                    target = board->lookup[label][1];
                else
                    target = board->lookup[label][0];

                bool is_outer = outer_edge(flatten_point3d(stack[i]), board);

                if (is_outer && stack[i].z > 0) {
                    if (!visited[stack[i].z - 1][target.y][target.x]) {
                        visited[stack[i].z - 1][target.y][target.x] = true;
                        next[next_sz++] = point3d_make(target.x, target.y, stack[i].z - 1);
                    }
                } else if (!is_outer && (stack[i].z + 1) < MAX_DEPTH) {
                    if (!visited[stack[i].z + 1][target.y][target.x]) {
                        visited[stack[i].z + 1][target.y][target.x] = true;
                        next[next_sz++] = point3d_make(target.x, target.y, stack[i].z + 1);
                    }
                }
            }
        }

        memcpy(stack, next, sizeof(*next) * next_sz);
        stack_sz = next_sz;
        next_sz = 0;
    }

    free(stack);
    free(next);
    free_ppp((void ***)visited, MAX_DEPTH, board->height);
    return -1;
}

char *day20p1(const char *input) {
    t_board board = parse_input(input);
    t_point2d start = board.lookup[encode_entrance('A', 'A')][0];
    t_point2d end = board.lookup[encode_entrance('Z', 'Z')][0];

    return aoc_asprintf("%d", bfs(&board, start, end));
}

char *day20p2(const char *input) {
    t_board board = parse_input(input);
    t_point2d start = board.lookup[encode_entrance('A', 'A')][0];
    t_point2d end = board.lookup[encode_entrance('Z', 'Z')][0];

    int steps = bfs_rec(&board, point3d_make(start.x, start.y, 0), point3d_make(end.x, end.y, 0));
    return aoc_asprintf("%d", steps);
}
