#include <stdlib.h>
#include "aoc.h"

void free_pp(void** pp, size_t size) {
    for (size_t i = 0; i < size; i++)
        free(pp[i]);
    free(pp);
}
