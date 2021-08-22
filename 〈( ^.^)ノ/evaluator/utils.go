package evaluator

import (
	"fmt"
	"os"
	"strconv"

	"github.com/grossamos/jam0001/shared"
)

func (e *Evaluator) eval_children(parentNode []shared.Node) []shared.Node {
	out := make([]shared.Node, len(parentNode))

	for index, subnode := range parentNode {
		out[index] = e.eval_expr(subnode)
	}

	return out
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

	return arr[varIndex]
}

func makeNumberNode(number string) shared.Node {
	return shared.Node{Val: shared.Token{
		Type:  shared.TTnumber,
		Value: number},
		IsExpression: false, Children: []shared.Node{}}
}
