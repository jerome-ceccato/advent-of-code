#include <stdio.h>
#include <stdlib.h>
#include "run.h"
#include "win.h"

int main(int argc, char** argv) {
    setupWindowsErrorHandling();

    if (argc > 1)
        run_one(atoi(argv[1]));
    else
        run_all();
    return 0;
}
