package evaluator

import (
	"os"
	"fmt"
	"strings"
	"github.com/grossamos/jam0001/shared"
)

func (e *Evaluator) eval_string_call(expr shared.Node) shared.Node {
	wholeComment := expr.Children[0].Val.Value

	andSplit := strings.Split(wholeComment, " and ")

	comments := []string{}
	for _, part := range andSplit {
		comments = append(comments, strings.Split(part, ",")...)
	}

	for i := range comments {
		comments[i] = strings.TrimSpace(comments[i])
	}

	args := make([]string, len(expr.Children[1].Children))

	for i, child := range expr.Children[1].Children {
		args[i] = e.eval_expr(child).Val.Value
	}

	ev := Evaluator{
		negative: []string{"0", "0", "0", "0", "0"},
		positive: args,
		zero: e.zero,
		comments: e.comments}

	for i, comment := range comments {
		_, ok := ev.comments[comment]
		if !ok {
			fmt.Println("Trying to call non existant comment")
			os.Exit(1)
		}

		ev.positive = ev.positive[ev.maxRef:]
		ev.maxRef = 0

		val := ev.eval_expr(ev.comments[comment])

		if i == len(comments) - 1 {
			e.zero = ev.zero
			return val
		}
	}

	return shared.Node{}
}
