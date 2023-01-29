#include <stdio.h>
#include <stdlib.h>
#include "aoc.h"

char* aoc_read_file(const char* filename) {
    FILE* file = fopen(filename, "r");
    char* buffer;
    size_t size;

    fseek(file, 0, SEEK_END);
    size = ftell(file);
    fseek(file, 0, SEEK_SET);

    buffer = malloc(size + 1);
    fread(buffer, 1, size, file);
    buffer[size] = '\0';
    fclose(file);

    return buffer;
}

void aoc_contents_to_ints(const char* data, char sep, int** out_array, size_t* out_size) {
    size_t number_of_sep = 0;
    for (int i = 0; data[i]; i++) {
        if (data[i] == sep)
            number_of_sep++;
    }

    *out_size = number_of_sep + 1;
    *out_array = malloc(sizeof(int) * *out_size);

    char num_buffer[128];
    int buf_i = 0, out_i = 0;
    for (; *data; ++data) {
        if (*data == sep) {
            num_buffer[buf_i] = 0;
            (*out_array)[out_i++] = atoi(num_buffer);
            buf_i = 0;
        } else {
            num_buffer[buf_i++] = *data;
        }
    }
    num_buffer[buf_i] = 0;
    (*out_array)[out_i++] = atoi(num_buffer);
}
