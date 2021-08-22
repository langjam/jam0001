package evaluator

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/grossamos/jam0001/shared"
)

func (e *Evaluator) eval_children(parentNode []shared.Node) []shared.Node {
	out := make([]shared.Node, len(parentNode))

	for index, subnode := range parentNode {
		out[index] = e.eval_expr(subnode)
	}

	return out
}

func (e *Evaluator) unrefString(str string) string {
	refString := ""

	for _, char := range str {
		if char == '$' {
			refString += string(char)
		} else if strings.Contains("-0123456789", string(char)) && len(refString) != 0 {
			refString += string(char)
		} else if refString != "" {
			str = strings.Replace(str, refString, e.getRefValue(refString[1:]), 1)
			refString = ""
		}
	}
	if refString != "" {
		str = strings.Replace(str, refString, e.getRefValue(refString[1:]), 1)
	}
	return str
}

func (e *Evaluator) getRefValue(ref string) string {
	varIndex, err := strconv.Atoi(ref)
	if err != nil {
		fmt.Println("Incorrect variable reference.")
		os.Exit(1)
	}

	if varIndex == 0 {
		return e.zero
	}

	arr := e.positive
	if varIndex < 0 {
		arr = e.negative
		varIndex *= -1
	}

	if len(arr) < varIndex {
		fmt.Println("Variable ref", ref, ": out of range.")
		os.Exit(1)
	}

	if varIndex > e.maxRef {
		e.maxRef = varIndex
	}

	return arr[varIndex-1]
}

func makeNumberNode(number string) shared.Node {
	return shared.Node{Val: shared.Token{
		Type:  shared.TTnumber,
		Value: number},
		IsExpression: false, Children: []shared.Node{}}
}
