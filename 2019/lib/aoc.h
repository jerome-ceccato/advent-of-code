#ifndef AOC_H
#define AOC_H

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

/*
 * Intcode
 */

typedef enum {
    INTCODE_RESULT_OK,
    INTCODE_RESULT_FAILURE
} t_intcode_result_status;

typedef struct {
    t_intcode_result_status status;  // Whether or not the program finished correctly
    int* memory;                     // The final memory state of the incode program
    size_t memory_size;              // The number of elements in `memory`
} t_intcode_result;

typedef void (*t_intcode_preprocessing)(int*, size_t);

// Evals an intcode program from a string
// `preprocessor` is an optional function that runs on the intcode memory before
// evaluation
t_intcode_result aoc_intcode_eval(const char* input,
                                  t_intcode_preprocessing preprocessor);

// Cleans up the memory allocated inside a `t_intcode_result`
void intcode_free_result(t_intcode_result* res);

#endif
