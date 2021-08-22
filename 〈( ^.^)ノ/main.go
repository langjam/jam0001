package main

import (
	"fmt"

	"io/ioutil"

	"github.com/grossamos/jam0001/evaluator"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
)

func main() {
	dat, err := ioutil.ReadFile("examples/fib.sml")
	if err != nil {
		panic(err)
	}

	code := string(dat)
	fmt.Println(code)

	toks := lexer.RunLexer(code)
	fmt.Println(toks)
	nodes, comments := parser.GenerateAst(toks)
	// nodes.Print("")
	evaluator.RunEvaluator(nodes, comments)
}
