package main

import (
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
	"github.com/grossamos/jam0001/shared"
)

func main() {
	toks := lexer.RunLexer("while 1")
	shared.Node{IsExpression: true, Children: parser.GenerateAst(&toks)}.Print("")
}
