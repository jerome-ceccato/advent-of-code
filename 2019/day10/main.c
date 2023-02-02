#define _USE_MATH_DEFINES
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "aoc.h"

// Returns an array of the encoded positions of asteroids
static void parse_asteroids(const char* input, int** out_map, size_t* out_size) {
    size_t width = strchr(input, '\n') - input;
    size_t height = strlen(input) / width;
    size_t map_size = aoc_occurences_of_char(input, '#');

    int* map = malloc(sizeof(*map) * map_size);
    size_t i = 0;
    for (size_t y = 0; y < height; y++) {
        for (size_t x = 0; x < width; x++) {
            if (*(input++) == '#')
                map[i++] = x * 100 + y;
        }
        input++;
    }

    *out_map = map;
    *out_size = map_size;
}

static inline int sq(int x) {
    return x * x;
}

static double distance(int lhs, int rhs) {
    return sqrt(sq((lhs / 100) - (rhs / 100)) + sq((lhs % 100) - (rhs % 100)));
}

static inline bool fequals(double lhs, double rhs) {
    return fabs(lhs - rhs) < 0.00001;
}

static bool can_see_asteroid(int* map, size_t size, int lhs, int rhs) {
    for (size_t i = 0; i < size; i++) {
        if (map[i] != lhs && map[i] != rhs) {
            if (fequals(distance(lhs, map[i]) + distance(rhs, map[i]), distance(lhs, rhs)))
                return false;
        }
    }
    return true;
}

static int detected_asteroids(int* map, size_t size, int pos) {
    int total = 0;
    for (size_t i = 0; i < size; i++) {
        if (map[i] != pos) {
            total += can_see_asteroid(map, size, pos, map[i]);
        }
    }
    return total;
}

static int station_location(int* map, size_t size, int* out_visible_asteroids) {
    int max_asteroids = 0;
    int pos = -1;
    for (size_t i = 0; i < size; i++) {
        int detected = detected_asteroids(map, size, map[i]);
        if (detected > max_asteroids) {
            max_asteroids = detected;
            pos = map[i];
        }
        max_asteroids = max(max_asteroids, detected);
    }

    *out_visible_asteroids = max_asteroids;
    return pos;
}

static int* visible_asteroids(int* map, size_t size, int station, int visible) {
    int* all = malloc(sizeof(*all) * visible);
    int alli = 0;
    for (size_t i = 0; i < size; i++) {
        if (map[i] != station) {
            if (can_see_asteroid(map, size, station, map[i])) {
                all[alli++] = map[i];
            }
        }
    }
    return all;
}

static double angle(int from, int to) {
    double t = atan2(to % 100 - from % 100, from / 100 - to / 100);
    double deg = t * (180.0 / M_PI);
    double fixed = 360 - (deg < 0 ? deg + 360 : deg) - 90;
    return fixed < 0 ? fixed + 360 : fixed;
}

static int compare_angle(int* station, const int* lhs, const int* rhs) {
    double a = angle(*station, *lhs);
    double b = angle(*station, *rhs);

    // Because they can be very small decimal values, we can't just cast the difference into an int
    if (a < b) {
        return -1;
    } else if (a > b) {
        return 1;
    }
    return 0;
}

// This is O(n^3)...
char* day10p1(const char* input) {
    int* map;
    size_t size;

    parse_asteroids(input, &map, &size);

    int max_asteroids = 0;
    station_location(map, size, &max_asteroids);

    free(map);
    return aoc_asprintf("%d", max_asteroids);
}

char* day10p2(const char* input) {
    int* map;
    size_t size;

    parse_asteroids(input, &map, &size);

    int max_asteroids = 0;
    int station = station_location(map, size, &max_asteroids);
    int* visible = visible_asteroids(map, size, station, max_asteroids);

    qsort_s(
        visible,
        max_asteroids,
        sizeof(*visible),
        (int (*)(void*, const void*, const void*))compare_angle,
        &station);

    int target = visible[200 - 1];

    free(map);
    free(visible);
    return aoc_asprintf("%d", target);
}
