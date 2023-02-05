#include <stdio.h>
#include <stdlib.h>
#include "run.h"
#include "aoc.h"

static const t_aoc_day days[] = {
    AOCDAY(1),
    AOCDAY(2),
    AOCDAY(3),
    AOCDAY(4),
    AOCDAY(5),
    AOCDAY(6),
    AOCDAY(7),
    AOCDAY(8),
    AOCDAY(9),
    AOCDAY(10),
    AOCDAY(11),
    AOCDAY(12),
    AOCDAY(13),
    AOCDAY(14),
    AOCDAY(15),
    AOCDAY(16),
};

void run(const t_aoc_day* day) {
    char filename[16];
    char* input;
    char* (*parts[])(const char*) = {day->p1, day->p2};

    sprintf_s(filename, sizeof filename, "day%02d/input", day->day);
    input = aoc_read_file(filename);

    printf("\nDay %d:\n", day->day);
    for (int i = 0; i < 2; i++) {
        char* p = (*parts[i])(input);
        if (p) {
            printf("- Part %d: %s\n", i + 1, p);
            free(p);
        }
    }
    free(input);
}

void run_all() {
    int all_days = sizeof days / sizeof(t_aoc_day);
    for (int i = 0; i < all_days; i++) {
        run(days + i);
    }
}

void run_one(int target) {
    int all_days = sizeof days / sizeof(t_aoc_day);
    for (int i = 0; i < all_days; i++) {
        if (days[i].day == target) {
            run(days + i);
            break;
        }
    }
}
