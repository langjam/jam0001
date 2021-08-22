package main

import (
	"fmt"
	"log"
	"time"

	"io/ioutil"
	"math/rand"

	"github.com/grossamos/jam0001/evaluator"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/parser"
	"github.com/grossamos/jam0001/shared"
)

func main() {
	rand.Seed(time.Now().Unix())

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
	nodes, comments, err := parser.GenerateAst(toks)
	shared.Node{IsExpression: true, Children: nodes}.Print("")
	if err != nil {
		log.Fatal(err)
	}

	// run evaluator
	evaluator.RunEvaluator(nodes, comments)
}
