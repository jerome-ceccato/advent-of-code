#ifndef AOC_H
#define AOC_H

#include "intcode.h"

/*
 * Input
 */

// Reads the entire file into a new string
char* aoc_read_file(const char* filename);

// Parses a string of ints separated by `sep` into an array
void aoc_contents_to_ints(const char* data, char sep, int** out_array, size_t* out_size);

/*
 * String
 */

// Returns a new string containing the formatted output
char* aoc_asprintf(const char* fmt, ...);

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

#endif
