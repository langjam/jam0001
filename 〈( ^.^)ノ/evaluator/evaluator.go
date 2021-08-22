package evaluator

import (
	"fmt"
	"os"

	"github.com/grossamos/jam0001/shared"
)

// some eval
type Evaluator struct {
	Nodes []shared.Node

	negative []string
	positive []string
	zero     string

	comments []string
}

func (e *Evaluator) eval_expr(expr shared.Node) shared.Node {
	if !expr.IsExpression {
		if expr.Val.Type == shared.TTref { // get reference
			expr.Val.Type = shared.TTstring
			expr.Val.Value = e.getRefValue(expr.Val.Value)
		}

		return expr
	}

	for i := 0; i < len(expr.Children); i++ { // for all children
		// if it is an expression, recursively evaluate it
		if expr.Children[i].IsExpression { // this fucking code makes the destruction
			if len(expr.Children) == 1 {
				return e.eval_expr(expr.Children[i])
			} else {
				e.eval_expr(expr.Children[i])
			}
		}

		switch expr.Children[i].Val.Type {
		case shared.TTinstruction:
			return e.eval_instruction(expr)

		case shared.TTstring:
			return e.eval_string_call(expr)

		case shared.TTnumber, shared.TTconst, shared.TTref:
			return expr.Children[i]

		case shared.TTwhile:
			return e.eval_while(expr)

		case shared.TTnull:

		case shared.TTwcomment, shared.TTwcommentAdd: // skip comments

		default:
			fmt.Println("Unimplemented feature:", expr.Val)
			os.Exit(1)
			return shared.Node{}
		}
	}

	return shared.Node{}
}

func RunEvaluator(nodes []shared.Node) {
	evaluator := Evaluator{nodes, []string{"0", "0", "0", "0"}, []string{}, "0", []string{}}
	for _, node := range nodes {
		evaluator.eval_expr(node)
	}
}
