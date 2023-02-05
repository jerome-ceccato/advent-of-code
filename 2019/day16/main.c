#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"

static void phase_once(char* input, char* output, size_t size) {
    int pattern[4] = {0, 1, 0, -1};

    for (size_t outi = 0; outi < size; outi++) {
        int value = 0;
        int patterni = 1;
        for (size_t ini = 0; ini < size; ini++) {
            int mul = pattern[(patterni++ / (outi + 1)) % 4];
            value += mul * (input[ini] - '0');
        }
        output[outi] = '0' + (abs(value) % 10);
    }
}

static char* apply_fft(const char* input, int n) {
    char* in = strdup(input);
    char* out = strdup(input);

    size_t size = strlen(input);
    for (int i = 0; i < n; i++) {
        phase_once(in, out, size);

        char* tmp = in;
        in = out;
        out = tmp;
    }

    free(out);
    return in;
}

static void trailing_phase_once(char* input, char* output, size_t size) {
    int value = 0;
    for (int outi = size - 1; outi >= 0; outi--) {
        value = (value + input[outi] - '0') % 10;
        output[outi] = '0' + value;
    }
}

static char* apply_trailing_fft(char* in, int n) {
    char* out = strdup(in);
    size_t size = strlen(in);

    for (int i = 0; i < n; i++) {
        trailing_phase_once(in, out, size);

        char* tmp = in;
        in = out;
        out = tmp;
    }

    free(out);
    return in;
}

char* day16p1(const char* input) {
    char* res = apply_fft(input, 100);
    res[8] = 0;
    return res;
}

char* day16p2(const char* input) {
    char* offset_s = strdup(input);
    offset_s[7] = 0;
    size_t offset = strtol(offset_s, NULL, 10);
    free(offset_s);

    size_t input_size = strlen(input);
    size_t full_size = input_size * 10000;
    size_t size = full_size - offset;
    size_t input_offset = offset % input_size;

    char* adjusted_input = malloc(size + 1);
    memcpy(adjusted_input, input + input_offset, input_size - input_offset);
    for (size_t i = input_size - input_offset; i < size; i += input_size)
        memcpy(adjusted_input + i, input, input_size);
    adjusted_input[size] = 0;

    apply_trailing_fft(adjusted_input, 100);
    adjusted_input[8] = 0;
    return adjusted_input;
}
