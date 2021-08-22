.PHONY: clean
clean:
	rm -f smal smal.o

smal: smal.s
	as -msyntax=intel -mnaked-reg $< -o $@.o
	ld $@.o -o $@