package evaluator

import (
	"github.com/grossamos/jam0001/shared"
)

// some eval
type Evaluator struct {
	Nodes []shared.Node

	negative []string
	positive []string
	zero     string

	maxRef   int
	comments map[string]shared.Node
}

func (e *Evaluator) eval_expr(expr shared.Node) (shared.Node, error) {

	var err error

	if !expr.IsExpression {
		if expr.Val.Type == shared.TTref { // get reference
			expr.Val.Type = shared.TTstring
			expr.Val.Value = e.getRefValue(expr.Val.Value)
		}

		return expr, err
	}

	for i := 0; i < len(expr.Children); i++ { // for all children
		// if it is an expression, recursively evaluate it
		if expr.Children[i].IsExpression {
			if i == len(expr.Children)-1 {
				return e.eval_expr(expr.Children[i])
			} else {
				_, err = e.eval_expr(expr.Children[i])
				if err != nil {
					return shared.Node{}, err
				}
			}
		}

		switch expr.Children[i].Val.Type {
		case shared.TTinstruction:
			return e.eval_instruction(expr)

		case shared.TTstring:
			return e.eval_string_call(expr)

		case shared.TTnumber, shared.TTconst:
			return expr.Children[i], err

		case shared.TTref:
			val, err := e.eval_expr(expr.Children[i])
			if err != nil {
				return val, err
			}

			if val.Val.Type == shared.TTstring && i < len(expr.Children) - 1 {
				return e.eval_string_call(
					shared.Node{
						IsExpression: true,
						Children: []shared.Node{val, expr.Children[i+1]}})
			
			}

			return val, nil

		case shared.TTwhile:
			return e.eval_while(expr)

		case shared.TTnull:

		case shared.TTwcomment, shared.TTwcommentAnd: // skip comments
			return shared.Node{}, err

		default:
			err = &UnimplementedError{message: "opperation either doesn't exist or isn't implemented so far", pos: expr.Val.Pos}
			return shared.Node{}, err
		}
	}

	return shared.Node{}, err
}

func RunEvaluator(nodes []shared.Node, comments map[string]shared.Node) error {
	evaluator := Evaluator{nodes, []string{"0", "0", "0", "0"}, []string{"0", "0", "0", "0"}, "0", 0, comments}
	for _, node := range nodes {
		_, err := evaluator.eval_expr(node)
		if err != nil {
			return err
		}
	}
	return nil
}
