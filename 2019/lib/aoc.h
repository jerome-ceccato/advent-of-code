#ifndef AOC_H
#define AOC_H

#include "utils.h"
#include "intcode.h"

/*
 * Input
 */

// Reads the entire file into a new string
char* aoc_read_file(const char* filename);

// Parses a string of ints separated by `sep` into an array
void aoc_contents_to_ints(const char* data, char sep, int** out_array, size_t* out_size);
void aoc_contents_to_bigints(const char* data, char sep, bigint** out_array, size_t* out_size);

/*
 * String
 */

// Returns a new string containing the formatted output
char* aoc_asprintf(const char* fmt, ...);

// bigint conversion
bigint str_to_bigint(char* str);
char* aoc_bigint_to_str(bigint a);

// Count the number of times `c` appears in `str`
size_t aoc_occurences_of_char(const char* str, char c);

/*
 * 2D
 */

typedef struct {
    int x;
    int y;
} t_point2d;

#define POINT2D_ZERO \
  (t_point2d) { 0, 0 }

t_point2d point2d_make(int x, int y);
t_point2d point2d_add(t_point2d lhs, t_point2d rhs);
int point2d_distance(t_point2d lhs, t_point2d rhs);

/*
 * 3D
 */

typedef struct {
    int x;
    int y;
    int z;
} t_point3d;

#define POINT3D_ZERO \
  (t_point3d) { 0, 0, 0 }

t_point3d point3d_make(int x, int y, int z);
t_point3d point3d_add(t_point3d lhs, t_point3d rhs);
bool point3d_eq(t_point3d lhs, t_point3d rhs);

/*
 * Math
 */

bigint aoc_gcd(bigint a, bigint b);
bigint aoc_lcm(bigint a, bigint b);

#endif
