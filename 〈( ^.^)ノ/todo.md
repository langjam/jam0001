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
	- [ ] - loops
	- [ ] - instructions
		- [ ] - m
			- [ ] - check correctness
				- [ ] - operation exists
				- [x] - is possible to evaluate
			- [x] - generate rpn
			- [x] - evaluate rpn
			- [ ] - functions

		- [ ] - set
		- [ ] - not
		- [x] - print
	- [ ] - referencing variables
	- [ ] - comments
		- [ ] - ast generation
		- [ ] - execution
			- [ ] - figure out what to do with stacks
