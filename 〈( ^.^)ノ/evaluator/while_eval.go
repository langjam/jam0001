package evaluator

import "github.com/grossamos/jam0001/shared"

func stringIsTrue(val string) bool {
	return val != "0" && val != "FALSE" && val != ""
}

func (e *Evaluator) eval_while(expr shared.Node) (shared.Node, error) {
	evaled_expr, err := e.eval_expr(expr.Children[1])
	if err != nil {
		return shared.Node{}, err
	}
	for stringIsTrue(evaled_expr.Val.Value) {
		_, err = e.eval_expr(expr.Children[2])
		if err != nil {
			return shared.Node{}, err
		}
	}

	return shared.Node{}, err
}
