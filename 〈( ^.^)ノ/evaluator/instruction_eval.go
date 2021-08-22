package evaluator

import (
	"fmt"
	"os"

	"github.com/grossamos/jam0001/evaluator/m"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/shared"
)

func (e *Evaluator) eval_instruction(expr shared.Node) shared.Node {
	instruction_type := expr.Children[0].Val.Value
	instruction_args := expr.Children[1].Children

	switch instruction_type {
	case lexer.II_set:
		set_ref := instruction_args[0]
		set_values := e.eval_children(instruction_args[1:])

		if set_ref.Val.Type != shared.TTref {
			fmt.Println("Cannot set non ref value.")
			os.Exit(1)
		}

		e.setRefValue(
			set_ref.Val.Value, set_values[0].Val.Value)

		return set_ref

	case lexer.II_print:
		args := e.eval_children(instruction_args)
		printString := args[0].Val.Value
		printString = e.unrefString(printString)
		fmt.Println(printString)

	case lexer.II_m:
		mathInput := instruction_args[0].Val.Value
		mathInput = e.unrefString(mathInput)
		mResult, err := m.Do(mathInput)
		if err != nil {
			fmt.Println("MathError: expression invalid: ", mathInput)
		}
		return makeNumberNode(mResult)
	}
	return shared.Node{}
}
