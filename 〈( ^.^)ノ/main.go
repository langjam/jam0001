package main

import (
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
	"github.com/grossamos/jam0001/shared"
)

func main() {
	toks := lexer.RunLexer("set($1 $2)\n" +
		"set($1 $2) // thing\n" +
		"while (1) {\n" +
		"	set($99 $-9)" +
		"}" +
		"set($12 $3)")
	shared.Node{IsExpression: true, Children: parser.GenerateAst(toks)}.Print("")
}
