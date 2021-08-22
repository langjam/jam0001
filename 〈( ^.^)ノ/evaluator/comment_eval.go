package evaluator

import (
	"strings"

	"github.com/grossamos/jam0001/shared"
)

func splitComments(input string) (comments []string) {
	andSplit := strings.Split(input, " and ")

	for _, part := range andSplit {
		comments = append(comments, strings.Split(part, ",")...)
	}

	return
}

func (e *Evaluator) eval_string_call(expr shared.Node) (shared.Node, error) {
	var err error
	wholeComment := e.unrefString(expr.Children[0].Val.Value)

	comments := splitComments(wholeComment)

	for i := range comments {
		comments[i] = strings.TrimSpace(comments[i])
	}

	args := make([]string, len(expr.Children[1].Children))

	for i, child := range expr.Children[1].Children {
		var evaled_expr shared.Node
		evaled_expr, err = e.eval_expr(child)
		if err != nil {
			return shared.Node{}, err
		}
		args[i] = evaled_expr.Val.Value
	}

	ev := Evaluator{
		negative: e.negative,
		positive: args,
		zero:     e.zero,
		comments: e.comments}

	val := shared.Node{}

	for _, comment := range comments {
		if comment == "" {
			continue
		}

		_, ok := ev.comments[comment]
		if !ok {
			return shared.Node{}, &EvalError{"Trying to call non existant comment.", expr.Val.Pos}
		}

		if ev.maxRef > len(ev.positive) {
			return shared.Node{}, &EvalError{"Not enough arguments", expr.Val.Pos}
		}

		ev.positive = ev.positive[ev.maxRef:]
		ev.maxRef = 0

		val, err = ev.eval_expr(ev.comments[comment])
		if err != nil {
			return shared.Node{}, err
		}
	}

	e.zero = ev.zero
	e.negative = ev.negative

	return val, err
}
