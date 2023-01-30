#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "aoc.h"

char* aoc_asprintf(const char* fmt, ...) {
    int ret;
    va_list lst;
    char buffer[65536];

    va_start(lst, fmt);
    ret = vsprintf_s(buffer, sizeof buffer, fmt, lst);
    va_end(lst);

    if (ret < 0 || ret >= 65536) {
        fprintf(stderr, "asprintf failed: %d", ret);
        return NULL;
    }
    return strdup(buffer);
}

size_t aoc_occurences_of_char(const char* str, char c) {
    size_t n = 0;
    for (int i = 0; str[i]; i++) {
        if (str[i] == c)
            n++;
    }
    return n;
}
