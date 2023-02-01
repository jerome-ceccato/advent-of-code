#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"
#include "utils.h"

static const size_t width = 25;
static const size_t height = 6;

static void parse_image(const char* input, unsigned char*** out_layers, size_t* out_nlayers) {
    size_t nlayers = strlen(input) / (width * height);

    unsigned char** layers = malloc(sizeof(*layers) * nlayers);
    for (size_t i = 0; i < nlayers; i++) {
        layers[i] = calloc(width * height, 1);
        for (size_t j = 0; j < (width * height); j++) {
            layers[i][j] = *(input++) - '0';
        }
    }
    *out_layers = layers;
    *out_nlayers = nlayers;
}

static void free_image(unsigned char** layers, size_t nlayers) {
    for (size_t i = 0; i < nlayers; i++)
        free(layers[i]);
    free(layers);
}

static int layer_count(unsigned char* layer, unsigned char target) {
    int total = 0;
    for (size_t i = 0; i < (width * height); i++) {
        if (layer[i] == target)
            total++;
    }
    return total;
}

char* day8p1(const char* input) {
    size_t nlayers;
    unsigned char** layers;

    parse_image(input, &layers, &nlayers);
    int min_0 = INT_MAX;
    int min_layer = -1;
    for (size_t i = 0; i < nlayers; i++) {
        int c = layer_count(layers[i], 0);
        if (c < min_0) {
            min_0 = c;
            min_layer = (int)i;
        }
    }

    int checksum = layer_count(layers[min_layer], 1)
                   * layer_count(layers[min_layer], 2);

    free_image(layers, nlayers);
    return aoc_asprintf("%d", checksum);
}

char* day8p2(const char* input) {
    size_t nlayers;
    unsigned char** layers;

    parse_image(input, &layers, &nlayers);
    unsigned char* final_layer = malloc(width * height);

    for (size_t i = 0; i < (width * height); i++) {
        size_t l = 0;
        while (layers[l][i] == 2)
            l++;
        final_layer[i] = layers[l][i];
    }

    char* output_str = malloc(width * (height + 1) + 2);
    size_t oi = 0;

    for (size_t i = 0; i < (width * height); i++) {
        if (!(i % width))
            output_str[oi++] = '\n';
        output_str[oi++] = final_layer[i] ? '#' : ' ';
    }
    output_str[oi] = 0;

    free(final_layer);
    free_image(layers, nlayers);
    return output_str;
}
