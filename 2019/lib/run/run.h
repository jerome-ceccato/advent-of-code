#ifndef RUN_H
#define RUN_H

#define DEFDAY(d)                      \
  char* day##d##p1(const char* input); \
  char* day##d##p2(const char* input);

typedef struct {
    int day;
    char* (*p1)(const char*);
    char* (*p2)(const char*);
} t_aoc_day;

#define AOCDAY(d) \
  { d, day##d##p1, day##d##p2 }

DEFDAY(1)
DEFDAY(2)
DEFDAY(3)
DEFDAY(4)
DEFDAY(5)
DEFDAY(6)

void run_all();
void run_one(int target);

#endif
