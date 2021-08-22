# todo list

- [x] - lexer
	- [x] - parenthesis
	- [x] - blocks
	- [x] - strings
	- [x] - instructions
	- [x] - numbers
	- [x] - constants
	- [x] - variable refs
	- [x] - comments

- [ ] - parser
	- [ ] - generate ast
		- [ ] - proper testing
	- [x] - instructions
		- [x] - check for argument count
	- [x] - while loop
	- [x] - string literals
	- [x] - number literals
	- [x] - constants
	- [x] - variables
	- [x] - comments

- [ ] - evaluator
	- [x] - loops
	- [ ] - instructions
		- [ ] - m
			- [x] - check correctness
				- [x] - operation exists
				- [x] - is possible to evaluate
			- [x] - generate rpn
			- [x] - evaluate rpn
			- [ ] - functions

		- [x] - set
		- [x] - not
		- [x] - print
		- [x] - inz
		- [x] - dnz
		- [x] - smile

	- [x] - referencing variables
	- [x] - replacing refs in strings
	- [ ] - comments
		- [x] - ast generation
		- [ ] - execution
			- [x] - figure out what to do with stacks
			- [ ] - fix returns if and used
- [ ] - meta
	- [ ] - replace occurances of ``fmt.Print(some_error)`` with ``log.Fatal()``
	- [ ] - update the shell
	- [ ] - maybe make main.go take in a file
