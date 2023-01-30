#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"
#include "utils.h"

t_point2d day3_dir_to_point(t_point2d prev, char dir, int value) {
    switch (dir) {
        case 'R':
            return point2d_add(prev, point2d_make(value, 0));
        case 'L':
            return point2d_add(prev, point2d_make(-value, 0));
        case 'U':
            return point2d_add(prev, point2d_make(0, -value));
        case 'D':
            return point2d_add(prev, point2d_make(0, value));
    }
    return prev;
}

void day3_parse_points(const char* input, t_point2d** out_points, size_t* out_size) {
    t_point2d prev = POINT2D_ZERO;
    char buffer[16];

    *out_size = aoc_occurences_of_char(input, ',') + 2;
    *out_points = malloc(sizeof(**out_points) * *out_size);
    **out_points = prev;

    size_t size = strlen(input);
    size_t out_i = 1;
    for (size_t i = 0; i < size; i++) {
        char dir = input[i++];
        size_t buf_i;

        for (buf_i = 0; i < size && input[i] != ','; i++) {
            buffer[buf_i++] = input[i];
        }
        buffer[buf_i] = 0;

        t_point2d this = day3_dir_to_point(prev, dir, atoi(buffer));
        (*out_points)[out_i++] = this;
        prev = this;
    }
}

void day3_parse_wires(const char* input, t_point2d* out_points[2], size_t out_size[2]) {
    char* mut = strdup(input);
    char* second = strchr(mut, '\n');
    *(second++) = 0;

    day3_parse_points(mut, out_points, out_size);
    day3_parse_points(second, out_points + 1, out_size + 1);

    free(mut);
}

// There's probably a smart way to do this
bool day3_intersection(t_point2d a1, t_point2d a2, t_point2d b1, t_point2d b2, t_point2d* intersection) {
    if (a1.x == a2.x && b1.y == b2.y) {
        int minbx = min(b1.x, b2.x);
        int maxbx = max(b1.x, b2.x);
        if (minbx <= a1.x && maxbx >= a1.x) {
            int minay = min(a1.y, a2.y);
            int maxay = max(a1.y, a2.y);
            if (minay <= b1.y && maxay >= b1.y) {
                if (a1.x == 0 && b1.y == 0) {
                    return false;
                }
                *intersection = point2d_make(a1.x, b1.y);
                return true;
            }
        }
    } else if (a1.y == a2.y && b1.x == b2.x) {
        int minby = min(b1.y, b2.y);
        int maxby = max(b1.y, b2.y);
        if (minby <= a1.y && maxby >= a1.y) {
            int minax = min(a1.x, a2.x);
            int maxax = max(a1.x, a2.x);
            if (minax <= b1.x && maxax >= b1.x) {
                if (b1.x == 0 && a1.y == 0) {
                    return false;
                }
                *intersection = point2d_make(b1.x, a1.y);
                return true;
            }
        }
    }
    return false;
}

char* day3p1(const char* input) {
    t_point2d* wires[2];
    size_t wire_sizes[2];

    day3_parse_wires(input, wires, wire_sizes);

    int min_distance = INT_MAX;
    t_point2d intersection;
    for (size_t a = 1; a < wire_sizes[0]; a++) {
        for (size_t b = 1; b < wire_sizes[1]; b++) {
            if (day3_intersection(wires[0][a - 1], wires[0][a], wires[1][b - 1], wires[1][b], &intersection)) {
                min_distance = min(min_distance, point2d_distance(intersection, POINT2D_ZERO));
            }
        }
    }

    return aoc_asprintf("%d", min_distance);
}

char* day3p2(const char* input) {
    t_point2d* wires[2];
    size_t wire_sizes[2];

    day3_parse_wires(input, wires, wire_sizes);

    int min_distance = INT_MAX;
    int steps[2] = {0, 0};
    t_point2d intersection;
    for (size_t a = 1; a < wire_sizes[0]; a++) {
        steps[1] = 0;
        for (size_t b = 1; b < wire_sizes[1]; b++) {
            if (day3_intersection(wires[0][a - 1], wires[0][a], wires[1][b - 1], wires[1][b], &intersection)) {
                int steps_to_inter = 0;
                steps_to_inter += steps[0] + point2d_distance(wires[0][a - 1], intersection);
                steps_to_inter += steps[1] + point2d_distance(wires[1][b - 1], intersection);
                min_distance = min(min_distance, steps_to_inter);
            }
            steps[1] += point2d_distance(wires[1][b - 1], wires[1][b]);
        }
        steps[0] += point2d_distance(wires[0][a - 1], wires[0][a]);
    }

    return aoc_asprintf("%d", min_distance);
}
