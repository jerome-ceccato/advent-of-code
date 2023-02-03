#include <stdlib.h>
#include "aoc.h"

t_point3d point3d_make(int x, int y, int z) {
    return (t_point3d){x, y, z};
}

t_point3d point3d_add(t_point3d lhs, t_point3d rhs) {
    return (t_point3d){lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z};
}

bool point3d_eq(t_point3d lhs, t_point3d rhs) {
    return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z;
}
