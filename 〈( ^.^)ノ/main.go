package main

import (
	"fmt"
	"log"
	"os"
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
	debugMode := false

	// do initial checks
	if len(os.Args) == 1 {
		fmt.Println("Please provide a file to run")
	}
	if len(os.Args) > 2 && contains(os.Args, "--debug") {
		debugMode = true
	}

	// load test code from disk
	dat, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	code := string(dat)
	if debugMode {
		fmt.Println(code)
	}

	// run lexer
	toks := lexer.RunLexer(code)
	if debugMode {
		fmt.Println(toks)
	}

	// run parser
	nodes, comments, err := parser.GenerateAst(toks)
	if debugMode {
		shared.Node{IsExpression: true, Children: nodes}.Print("")
	}
	if err != nil {
		log.Fatal(err)
	}

	// run evaluator
	err = evaluator.RunEvaluator(nodes, comments)
	if err != nil {
		log.Fatal(err)
	}
}

func contains(s []string, e string) bool {
	for _, a := range s {
		if a == e {
			return true
		}
	}
	return false
}
