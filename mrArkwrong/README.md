# Tracecom

Tracecom is a simple, interpreted language that supports comments which can be used for program execution tracing. Every comment in Tracecom belongs to an expression. E.g. in `(7 + 10)<an addition> - 3` the comment `an addition` belongs to the expression `7 + 10`. Comments can have the trace flag `#` like in `(7 + 10)<#an addition> - 3`. Everytime an expression which has a comment with the trace flag is evaluated, its computed result value together with the comment will be printed to the command line. 

# Building and Executing

`sbt run` builds the interpreter and runs the program in _example.txt_