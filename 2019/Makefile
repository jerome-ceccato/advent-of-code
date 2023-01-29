NAME := aoc
CC := gcc
# Windows...
RM := powershell.exe .\clean.ps1

SRCS := $(wildcard main.c lib/*.c lib/run/*.c day*/*.c)

OBJS = $(SRCS:.c=.o)
INCLUDES = -Ilib -Ilib/run
CFLAGS = -W -Wall $(INCLUDES)

$(NAME) : $(OBJS)
	$(CC) -o $(NAME) $(OBJS)

.PHONY: fclean clean re

clean :
	$(RM) $(OBJS)

fclean : clean
	$(RM) $(NAME).exe

re : fclean $(NAME)
