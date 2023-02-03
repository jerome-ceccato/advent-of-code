#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"

static void parse_input(const char* input, t_point3d pos[4]) {
    for (int line = 0; line < 4; line++) {
        input += strlen("<x=");
        pos[line].x = (int)strtol(input, (char**)&input, 10);
        input += strlen(", y=");
        pos[line].y = (int)strtol(input, (char**)&input, 10);
        input += strlen(", z=");
        pos[line].z = (int)strtol(input, (char**)&input, 10);
        input += strlen(">\n");
    }
}

static int offset(int lhs, int rhs) {
    return lhs > rhs ? -1 : (lhs < rhs ? 1 : 0);
}

static void simulate_motion(t_point3d pos[4], t_point3d velocity[4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = i + 1; j < 4; j++) {
            int offx = offset(pos[i].x, pos[j].x);
            int offy = offset(pos[i].y, pos[j].y);
            int offz = offset(pos[i].z, pos[j].z);

            velocity[i].x += offx;
            velocity[i].y += offy;
            velocity[i].z += offz;
            velocity[j].x -= offx;
            velocity[j].y -= offy;
            velocity[j].z -= offz;
        }
    }

    for (int i = 0; i < 4; i++) {
        pos[i] = point3d_add(pos[i], velocity[i]);
    }
}

static inline int energy(t_point3d p) {
    return abs(p.x) + abs(p.y) + abs(p.z);
}

static int total_energy(t_point3d pos[4], t_point3d velocity[4]) {
    int total = 0;
    for (int i = 0; i < 4; i++) {
        total += energy(pos[i]) * energy(velocity[i]);
    }
    return total;
}

static void simulation_eq(t_point3d lpos[4], t_point3d lv[4], t_point3d rpos[4], t_point3d rv[4], bool out[3]) {
    for (int i = 0; i < 3; i++)
        out[i] = true;

    for (int i = 0; i < 4; i++) {
        if (lpos[i].x != rpos[i].x || lv[i].x != rv[i].x)
            out[0] = false;
        if (lpos[i].y != rpos[i].y || lv[i].y != rv[i].y)
            out[1] = false;
        if (lpos[i].z != rpos[i].z || lv[i].z != rv[i].z)
            out[2] = false;
    }
}

char* day12p1(const char* input) {
    t_point3d pos[4], velocity[4] = {0};

    parse_input(input, pos);

    for (int i = 0; i < 1000; i++)
        simulate_motion(pos, velocity);

    return aoc_asprintf("%d", total_energy(pos, velocity));
}

char* day12p2(const char* input) {
    t_point3d pos[4], orig[4], velocity[4] = {0}, origv[4] = {0};

    parse_input(input, pos);
    parse_input(input, orig);

    t_point3d cycle = point3d_make(-1, -1, -1);
    int steps = 0;
    while (cycle.x < 0 || cycle.y < 0 || cycle.z < 0) {
        simulate_motion(pos, velocity);
        steps++;

        bool eq[3];
        simulation_eq(pos, velocity, orig, origv, eq);

        if (cycle.x < 0 && eq[0])
            cycle.x = steps;
        if (cycle.y < 0 && eq[1])
            cycle.y = steps;
        if (cycle.z < 0 && eq[2])
            cycle.z = steps;
    }

    return aoc_bigint_to_str(aoc_lcm(aoc_lcm(cycle.x, cycle.y), cycle.z));
}
