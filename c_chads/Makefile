.PHONY: clean, test
CSAN := #-fsanitize=address -fsanitize=undefined
CWARN := -Wconversion -Wsign-conversion -Wextra -Wall -Wno-initializer-overrides -pedantic
CSHD := $(shell find aid -name '*.c') $(shell find lang -name '*.c') 
CSRC := $(CSHD) $(shell find frontend -name '*.c')
CTST := $(CSHD) ./tests/lib/test.c ./tests/runner.c
# May improve compile times if you have ccache installed
CPFX := $(shell command -v ccache)
CLIB := -lc -lm 
CC := clang
CFLAGS := $(CSAN) $(CLIB) $(CWARN) -g -Werror

all:
	if [ ! -d "./bin" ]; then mkdir bin; fi
	$(CPFX) $(CC) $(CFLAGS) $(CSRC) -o ./bin/COLAN-21

test:
	$(CPFX) $(CC) $(CFLAGS) $(CTST) -Wno-gnu-zero-variadic-macro-arguments -g -o ./bin/test
	./bin/test

clean:
	if [ -d "./bin" ]; then rm -r ./bin; fi

