package evaluator

import "github.com/grossamos/jam0001/shared"

func stringIsTrue(val string) bool {
	return val != "0" && val != "FALSE" && val != ""
}

func (e *Evaluator) eval_while(expr shared.Node) shared.Node {
	for stringIsTrue(e.eval_expr(expr.Children[1]).Val.Value) {
		e.eval_expr(expr.Children[2])
	}

	return shared.Node{}
}
