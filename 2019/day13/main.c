#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aoc.h"

// Define this to enable output and step by step solution
#undef AOC_13_INTERACTIVE

typedef enum {
    TILE_EMPTY,
    TILE_WALL,
    TILE_BLOCK,
    TILE_PADDLE,
    TILE_BALL
} t_tile;

static t_intcode_result run_intcode(const char* input, bigint mode) {
    t_intcode_state state = aoc_intcode_boot(input);
    state.memory.data[0] = mode;
    aoc_intcode_upgrade_memory(&state, 4096);

    return aoc_intcode_eval(state);
}

static void get_screen_size(t_intcode_state* state, size_t* out_width, size_t* out_height) {
    *out_width = 0;
    *out_height = 0;
    for (size_t i = 0; (i + 1) < state->output.head; i += 3) {
        *out_width = max(*out_width, (size_t)state->output.data[i]);
        *out_height = max(*out_height, (size_t)state->output.data[i + 1]);
    }
    // Size is highest position + 1 (starts at 0)
    ++*out_width;
    ++*out_height;
}

static t_tile** build_screen(size_t width, size_t height) {
    t_tile** screen = malloc(sizeof(*screen) * height);
    for (size_t i = 0; i < height; i++)
        screen[i] = calloc(width, sizeof(**screen));
    return screen;
}

static void free_screen(t_tile** screen, size_t height) {
    for (size_t i = 0; i < height; i++)
        free(screen[i]);
    free(screen);
}

static void update_game_state(t_tile** screen, bigint* score, t_intcode_state* state, t_point2d* paddle, t_point2d* ball) {
    for (size_t i = 0; (i + 2) < state->output.head; i += 3) {
        if (state->output.data[i] == -1) {
            *score = state->output.data[i + 2];
        } else {
            int x = state->output.data[i];
            int y = state->output.data[i + 1];
            int tile = state->output.data[i + 2];

            screen[y][x] = tile;
            if (tile == TILE_PADDLE) {
                paddle->x = x;
                paddle->y = y;
            } else if (tile == TILE_BALL) {
                ball->x = x;
                ball->y = y;
            }
        }
    }
}

#ifdef AOC_13_INTERACTIVE
static char tile_to_c(t_tile tile) {
    return " #@-*"[tile];
}

static void print_screen(t_tile** screen, size_t width, size_t height) {
    for (size_t y = 0; y < height; y++) {
        for (size_t x = 0; x < width; x++) {
            putchar(tile_to_c(screen[y][x]));
        }
        putchar('\n');
    }
}

static void print_game(t_tile** screen, size_t width, size_t height, bigint score) {
    print_screen(screen, width, height);
    char* score_s = aoc_bigint_to_str(score);
    printf("Score: %s\n", score_s);
    free(score_s);
}
#endif

static int get_next_move(t_point2d paddle, t_point2d ball) {
    if (paddle.x < ball.x)
        return 1;
    else if (paddle.x > ball.x)
        return -1;
    return 0;
}

char* day13p1(const char* input) {
    t_intcode_result result = run_intcode(input, 1);

    int nblocks = 0;
    for (size_t i = 2; i < result.state.output.head; i += 3) {
        if (result.state.output.data[i] == 2) {
            nblocks++;
        }
    }

    intcode_free_result(&result);
    return aoc_asprintf("%d", nblocks);
}

char* day13p2(const char* input) {
    t_intcode_result initial_result = run_intcode(input, 1);
    t_tile** screen;
    size_t width, height;
    bigint score = 0;
    t_point2d paddle = POINT2D_ZERO, ball = POINT2D_ZERO;

    get_screen_size(&initial_result.state, &width, &height);
    screen = build_screen(width, height);
    update_game_state(screen, &score, &initial_result.state, &paddle, &ball);
    intcode_free_result(&initial_result);

    t_intcode_result game = run_intcode(input, 2);
    game.state.input.data = malloc(sizeof(*game.state.input.data));
    game.state.input.size = 1;
    while (game.status == INTCODE_RESULT_FAILURE) {
        update_game_state(screen, &score, &game.state, &paddle, &ball);

        game.state.input.data[0] = get_next_move(paddle, ball);
        game.state.input.head = 0;
        game.state.output.head = 0;

#ifdef AOC_13_INTERACTIVE
        print_game(screen, width, height, score);
        getchar();
#endif
        game = aoc_intcode_eval(game.state);
    }

    update_game_state(screen, &score, &game.state, &paddle, &ball);
#ifdef AOC_13_INTERACTIVE
    print_game(screen, width, height, score);
#endif

    intcode_free_result(&game);
    free_screen(screen, height);
    return aoc_bigint_to_str(score);
}
