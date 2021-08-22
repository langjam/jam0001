package main

import (
	"fmt"
	"time"

	"io/ioutil"
	"math/rand"

	"github.com/grossamos/jam0001/evaluator"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
)

func main() {
	rand.Seed(time.Now().Unix())

	dat, err := ioutil.ReadFile("examples/test_while.sml")
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
