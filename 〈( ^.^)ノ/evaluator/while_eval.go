package evaluator

import (
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/shared"
)

func stringIsTrue(val string) bool {
	return val != "0" && val != "FALSE" && val != ""
}

func (e *Evaluator) eval_while(expr shared.Node) (shared.Node, error) {
	hasBreak := hasBreak(expr)

	for  {
		evaled_expr, err := e.eval_expr(expr.Children[1])
		if err != nil {
			return shared.Node{}, err
		}

		if !stringIsTrue(evaled_expr.Val.Value) {
			break
		}

		_, err = e.eval_expr(expr.Children[2])
		if err != nil {
			return shared.Node{}, err
		} else if hasBreak {
			return shared.Node{}, err
		}
	}

	return shared.Node{}, nil
}

func hasBreak(expr shared.Node) (exists bool) {
	for _, child := range expr.Children {
		if child.Val.Value == lexer.II_break {
			exists = true
			return
		}
		if !isWhileLoopParent(child) && hasBreak(child) {
			exists = true
			return
		}
	}
	return
}

func isWhileLoopParent(possibleWhileLoopParent shared.Node) bool {
	return len(possibleWhileLoopParent.Children) > 0 && possibleWhileLoopParent.Children[0].Val.Type == shared.TTwhile
}
