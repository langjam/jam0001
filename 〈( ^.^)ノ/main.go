package main

import (
	"fmt"
	"log"

	"io/ioutil"

	"github.com/grossamos/jam0001/evaluator"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
	"github.com/grossamos/jam0001/shared"
)

func main() {
	// load test code from disk
	dat, err := ioutil.ReadFile("examples/test_while.sml")
	if err != nil {
		log.Fatal(err)
	}
	code := string(dat)
	fmt.Println(code)

	// run lexer
	toks := lexer.RunLexer(code)
	fmt.Println(toks)

	// run parser
	nodes, comments := parser.GenerateAst(toks)
	shared.Node{IsExpression: true, Children: nodes}.Print("")

	// run evaluator
	evaluator.RunEvaluator(nodes, comments)
}
