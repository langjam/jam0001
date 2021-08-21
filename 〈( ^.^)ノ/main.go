package main

import (
	"fmt"

	"io/ioutil"

	"github.com/grossamos/jam0001/evaluator"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
	"github.com/grossamos/jam0001/shared"
)

func main() {
	dat, err := ioutil.ReadFile("examples/test_m.sml")
	if err != nil {
		panic(err)
	}

	code := string(dat)
	fmt.Println(code)

	toks := lexer.RunLexer(code)
	fmt.Println(toks)
	nodes := shared.Node{IsExpression: true, Children: parser.GenerateAst(toks)}
	// nodes.Print("")
	evaluator.RunEvaluator(nodes.Children)
}
