#include <stdlib.h>
#include "aoc.h"

void free_pp(void** pp, size_t size) {
    for (size_t i = 0; i < size; i++)
        free(pp[i]);
    free(pp);
}

void free_ppp(void*** ppp, size_t size1, size_t size2) {
    for (size_t i = 0; i < size1; i++) {
        for (size_t j = 0; j < size2; j++)
            free(ppp[i][j]);
        free(ppp[i]);
    }
    free(ppp);
}
