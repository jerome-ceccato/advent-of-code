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
