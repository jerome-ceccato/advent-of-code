#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "aoc.h"
#include "utils.h"

char* day9p1(const char* input) {
    t_intcode_result result = aoc_intcode_eval(input, NULL);
    intcode_print_output(&result.state);
    intcode_free_result(&result);
    return NULL;
}

char* day9p2(const char* input) {
    return NULL;
}
