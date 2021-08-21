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
	// recursibve calls to eval sub-nodes

	if expr.Val.Type == shared.TTnull && len(expr.Children) > 0 && expr.Children[0].Val.Type == shared.TTinstruction && expr.Children[len(expr.Children)-1].Val.Type != shared.TTwcomment {
		// Deal with instructions
		if expr.Children[0].Val.Value == lexer.II_set {
			// TODO catch if set doesn;t have enough children (maybe, or in the parser, idk)
			e.eval_children(&expr.Children[1])

			varIndex, err := strconv.Atoi(expr.Children[1].Children[0].Val.Value)
			varIndex *= -1
			val := expr.Children[1].Children[1].Val.Value
			for len(e.vars) <= varIndex {
				e.vars = append(e.vars, "0")
			}
			e.vars[varIndex] = val
			if err != nil {
				fmt.Println("TypeError, type used is invalid!", expr.Children[1].Val.Value, expr.Children[2].Val.Value)
				os.Exit(1)
			}
		} else if expr.Children[0].Val.Value == lexer.II_print {
			// TODO: catch it being empty
			var prnt_out string
			if expr.Children[1].Children[0].Val.Type == shared.TTref {
				prnt_out = e.getRefValue(expr.Children[1].Children[0])
			} else {
				prnt_out = expr.Children[1].Children[0].Val.Value
			}
			fmt.Println(prnt_out)
		}
		return shared.Node{}
	} else {
		fmt.Println("Illegal Expression ERROR", expr.Val)
		return shared.Node{}
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

func (e *Evaluator) eval_children(parentNode *shared.Node) {
	for index, subnode := range parentNode.Children {
		if subnode.IsExpression {
			parentNode.Children[index] = e.eval_expr(subnode)
		}
	}
}

func RunEvaluator(nodes []shared.Node) {
	evaluator := Evaluator{nodes, []string{}, []string{}}
	for _, node := range nodes {
		evaluator.eval_expr(node)
	}
}
