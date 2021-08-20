CC=gcc
CFLAGS=-Wall -std=c99
NAME=ech
RM=rm -f

all: clean $(NAME)

$(NAME): src/lexer.c src/parser.c src/main.c
	$(CC) -o $(NAME) $^

clean:
	$(RM) $(NAME)
