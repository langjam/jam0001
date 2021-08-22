#include <stdlib.h>
#include <stdio.h>
#include "compiler.h"

int main() {
	machine_t machine;
	compiler_t* compiler = malloc(sizeof(compiler_t));
	init_compiler(compiler, "global auto add = int(int a, int b) { if(a == b) { return a + b } else { int k = 20 bool i = false return 0 } } int sum = add(32, 84)");
	compile(compiler, &machine);

	return 0;
}