package main

import (
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
	"github.com/grossamos/jam0001/shared"
)

func main() {
	toks := lexer.RunLexer("set($1 $2) set($1 $2) // thing")
	shared.Node{IsExpression: true, Children: parser.GenerateAst(&toks)}.Print("")
}
