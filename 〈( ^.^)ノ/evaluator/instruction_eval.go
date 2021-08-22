package evaluator

import (
	"fmt"
	"log"

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
			log.Fatal(EvalError{message: "set can only be used on tape", pos: expr.Val.Pos})
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
			log.Fatal(MathEvalError{message: "expression: \"" + mathInput + "\"produced error: " + err.Error(), pos: expr.Val.Pos})
		}
		return makeNumberNode(mResult)

	case lexer.II_not:
		input := instruction_args[0].Val.Value

		out := ""

		switch input {
		case "TRUE":
			out = "FALSE"
		case "FALSE":
			out = "TRUE"
		case "0":
			out = "1"
		default:
			out = "0"
		}

		instruction_args[0].Val.Value = out
		return instruction_args[0]
	}
	return shared.Node{}
}
