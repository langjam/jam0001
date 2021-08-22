#!/bin/sh

g++ -std=c++17 -pedantic -Wall -Wextra -g -o rjl Common.cpp Main.cpp Lexer.cpp Parser.cpp Interpreter.cpp
