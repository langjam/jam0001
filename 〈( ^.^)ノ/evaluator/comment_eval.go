package evaluator

import (
	"os"
	"fmt"
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

func (e *Evaluator) eval_string_call(expr shared.Node) shared.Node {
	wholeComment := e.unrefString(expr.Children[0].Val.Value)

	comments := splitComments(wholeComment)

	for i := range comments {
		comments[i] = strings.TrimSpace(comments[i])
	}

	args := make([]string, len(expr.Children[1].Children))

	for i, child := range expr.Children[1].Children {
		args[i] = e.eval_expr(child).Val.Value
	}

	ev := Evaluator{
		negative: e.negative,
		positive: args,
		zero: e.zero,
		comments: e.comments}

	val := shared.Node{}

	for _, comment := range comments {
		_, ok := ev.comments[comment]
		if !ok {
			fmt.Println("Trying to call non existant comment.")
			os.Exit(1)
		}

		if ev.maxRef > len(ev.positive) {
			fmt.Println("Not enough arguments.")
			os.Exit(1)
		}

		ev.positive = ev.positive[ev.maxRef:]
		ev.maxRef = 0

		val = ev.eval_expr(ev.comments[comment])
	}

	e.zero = ev.zero
	e.negative = ev.negative

	return val
}
