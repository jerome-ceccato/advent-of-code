#include <stdlib.h>
#include "aoc.h"

t_point2d point2d_make(int x, int y) {
    return (t_point2d){x, y};
}

t_point2d point2d_add(t_point2d lhs, t_point2d rhs) {
    return (t_point2d){lhs.x + rhs.x, lhs.y + rhs.y};
}

int point2d_distance(t_point2d lhs, t_point2d rhs) {
    return abs(lhs.x - rhs.x) + abs(lhs.y - rhs.y);
}

bool point2d_eq(t_point2d lhs, t_point2d rhs) {
    return lhs.x == rhs.x && lhs.y == rhs.y;
}
