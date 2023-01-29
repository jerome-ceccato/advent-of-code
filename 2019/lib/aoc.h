#ifndef AOC_H
#define AOC_H

/*
 * Input
 */

// Reads the entire file into a new string
char* aoc_read_file(const char* filename);

// Parses a string of ints separated by "sep" into an array
void aoc_contents_to_ints(const char* data, char sep, int** out_array, size_t* out_size);

/*
 * String
 */

// Returns a new string containing the formatted output
char* aoc_asprintf(const char* fmt, ...);

#endif
