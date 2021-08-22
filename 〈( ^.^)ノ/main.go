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

func help() {
	fmt.Println(
		`smile - an esoteric programming language made for langjam
Usage: smile [ file ] [ flags ]
	--help, -h: show this help and exit
	--debug, -d: show info for debugging the interpretor (ast, tokens)`)

	os.Exit(0)
}

func main() {
	rand.Seed(time.Now().Unix())
	debugMode := false
	file := ""

	// do initial checks
	if len(os.Args) == 1 {
		help()
	}

	for i := 1; i < len(os.Args); i++ {
		switch os.Args[i] {
		case "--version":
			fmt.Println("〈( ^.^)ノ\tv0.1")
			return
		case "--debug", "-d":
			debugMode = true
		case "--help", "-h":
			help()
		default:
			if file == "" {
				file = os.Args[i]
			} else {
				fmt.Println("Incorrect flags.")
				return
			}
		}
	}

	// load test code from disk
	dat, err := ioutil.ReadFile(file)
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
