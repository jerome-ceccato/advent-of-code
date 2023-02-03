#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"

#define SIZE 200

typedef enum {
    UP,
    RIGHT,
    DOWN,
    LEFT
} t_direction;

static void move_robot(bool map[SIZE][SIZE], t_point2d* robot, t_direction* dir, bool visited[SIZE * SIZE], bool color, bool turn) {
    map[robot->y][robot->x] = color;
    visited[robot->y * SIZE + robot->x] = true;

    *dir = (*dir + (turn ? 1 : 3)) % 4;
    switch (*dir) {
        case UP:
            robot->y--;
            break;
        case RIGHT:
            robot->x++;
            break;
        case DOWN:
            robot->y++;
            break;
        case LEFT:
            robot->x--;
            break;
    }
}

static void paint(const char* input, bool map[SIZE][SIZE], bool visited[SIZE * SIZE], t_point2d robot) {
    t_intcode_state initial_state = aoc_intcode_boot(input);
    aoc_intcode_upgrade_memory(&initial_state, 2048);
    t_intcode_result result = aoc_intcode_eval(initial_state);
    t_direction dir = UP;

    while (result.status == INTCODE_RESULT_FAILURE) {
        intcode_set_input1(&result.state, map[robot.y][robot.x]);
        result = aoc_intcode_eval(result.state);

        if (result.state.output.head == 2) {
            bool color = result.state.output.data[0];
            bool turn = result.state.output.data[1];
            move_robot(map, &robot, &dir, visited, color, turn);
            result.state.output.head = 0;
        }
    }
}

static char* print_submap(bool map[SIZE][SIZE], t_point2d top_left, t_point2d bottom_right) {
    size_t size = (bottom_right.y - top_left.y) * (bottom_right.x - top_left.x + 1);
    char* result = malloc(size + 1);
    size_t i = 0;

    for (int y = top_left.y; y < bottom_right.y; y++) {
        result[i++] = '\n';
        for (int x = top_left.x; x < bottom_right.x; x++) {
            result[i++] = map[y][x] ? '#' : ' ';
        }
    }
    result[i] = 0;
    return result;
}

char* day11p1(const char* input) {
    bool map[SIZE][SIZE] = {0};
    bool visited[SIZE * SIZE] = {0};
    t_point2d robot = point2d_make(SIZE / 2, SIZE / 2);

    paint(input, map, visited, robot);

    int total_visited = 0;
    for (size_t i = 0; i < sizeof visited / sizeof(*visited); i++) {
        total_visited += visited[i];
    }

    return aoc_asprintf("%d", total_visited);
}

char* day11p2(const char* input) {
    bool map[SIZE][SIZE] = {0};
    bool visited[SIZE * SIZE] = {0};
    t_point2d robot = point2d_make(SIZE / 2, SIZE / 2);

    map[robot.y][robot.x] = 1;
    paint(input, map, visited, robot);

    // I _could_ find these values programatically, but I can also just hardcode them...
    return print_submap(map, point2d_make(100, 100), point2d_make(140, 106));
}
