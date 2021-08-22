package evaluator

import (
	"fmt"
	"os"
	"math/rand"
	"strconv"
	"strings"

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
			os.Exit(1)
		}
		return makeNumberNode(mResult)

	case lexer.II_not:
		args := e.eval_children(instruction_args)
		input := args[0].Val.Value

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

	case lexer.II_smile:
		smiles := []string{"〈( ^.^)ノ", "(>̯-̮<̯)", "(/^▽^)/", "(⌐■_■)", "( ¬_¬)"}

		return makeStringNode(smiles[rand.Int()%len(smiles)])

	case lexer.II_inz:
		args := e.eval_children(instruction_args)
		if !stringIsTrue(args[0].Val.Value) {
			break
		}

		split := splitComments(args[1].Val.Value)	

		pos, err := strconv.Atoi(args[2].Val.Value)
		if err != nil {
			// TODO
		}

		// pass by reference hell
		out := make([]string, len(split))
		copy(out, split)
		out = append(
			out[:pos],
			args[3].Val.Value)
		out = append(out, split[pos:]...)

		return makeStringNode(strings.Join(out, ","))
	case lexer.II_dnz:
		args := e.eval_children(instruction_args)
		if !stringIsTrue(args[0].Val.Value) {
			break
		}

		split := splitComments(args[1].Val.Value)	

		pos, err := strconv.Atoi(args[2].Val.Value)
		if err != nil {
			// TODO
		}

		if pos >= len(split) -1 {
			break
		}

		split = append(split[:pos], split[pos+1:]...)

		return makeStringNode(strings.Join(split, ","))
	}
	return shared.Node{}
}
