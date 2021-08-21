package main

import (
	"fmt"

	"github.com/grossamos/jam0001/evaluator"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
	"github.com/grossamos/jam0001/shared"
)

func main() {
	toks := lexer.RunLexer("set($-1 420)\n print($-1)")
	fmt.Println(toks)
	nodes := shared.Node{IsExpression: true, Children: parser.GenerateAst(toks)}
	// nodes.Print("")
	evaluator.RunEvaluator(nodes.Children)
}
