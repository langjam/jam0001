CC=gcc
LINK=-lm
CFLAGS=-Wall -Wextra -Werror -std=c99
NAME=ech
RM=rm -f

all: clean $(NAME)

$(NAME): src/lexer.c src/parser.c src/varmap.c src/interpreter.c src/main.c
	$(CC) -o $(NAME) $^ $(LINK)

clean:
	$(RM) $(NAME)
