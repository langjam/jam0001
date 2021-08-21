package evaluator

import (
	"fmt"
	"os"
	"strconv"

	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/shared"
)

// some eval
type Evaluator struct {
	Nodes    []shared.Node
	vars     []string
	comments []string
}

func (e *Evaluator) eval_expr(expr shared.Node) shared.Node {
	// Deal with instructions
	if isInstruction(expr.Children[0], expr.Children[1]) {
		instruction_type := expr.Children[0].Val.Value
		instruction_args := expr.Children[1].Children

		switch instruction_type {
		case lexer.II_set:
			// TODO: add error if args aren't two and one isn't ref
			set_values := instruction_args[1:]
			set_ref := instruction_args[0]
			e.eval_children(&set_values)

			varsIndex, err := strconv.Atoi(set_ref.Val.Value)
			varsIndex *= -1

			if err != nil {
				fmt.Println("TypeError, type used is invalid!", set_ref.Val.Value)
				os.Exit(1)
			}

			// fill up vars and set it
			for len(e.vars) <= varsIndex {
				e.vars = append(e.vars, "0")
			}
			e.vars[varsIndex] = set_values[0].Val.Value

		case lexer.II_print:
			for _, arg := range instruction_args {
				if arg.Val.Type == shared.TTref {
					fmt.Println(e.getRefValue(arg))
				} else {
					fmt.Println(arg.Val.Value)
				}
			}
		}
		return shared.Node{}

	} else {
		fmt.Println("Illegal Expression ERROR", expr.Val)
		os.Exit(1)
		return shared.Node{}
	}
}

func isInstruction(possible_instruction shared.Node, args shared.Node) bool {
	return possible_instruction.Val.Type == shared.TTinstruction &&
		len(args.Children) > 0
}

func (e *Evaluator) eval_children(parentNode *[]shared.Node) {
	for index, subnode := range *parentNode {
		if subnode.IsExpression {
			(*parentNode)[index] = e.eval_expr(subnode)
		}
	}
}

func (e *Evaluator) getRefValue(expr shared.Node) string {
	varIndex, err := strconv.Atoi(expr.Val.Value)
	varIndex *= -1
	if err != nil {
		fmt.Println("TypeError, type used is invalid!", expr.Val.Value)
		os.Exit(1)
	}
	// TODO catch if vars isn't big enough
	return e.vars[varIndex]
}

func RunEvaluator(nodes []shared.Node) {
	evaluator := Evaluator{nodes, []string{}, []string{}}
	for _, node := range nodes {
		evaluator.eval_expr(node)
	}
}
