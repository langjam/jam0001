package evaluator

import (
	"fmt"
	"log"
	"math/rand"
	"strconv"
	"strings"

	"github.com/grossamos/jam0001/evaluator/m"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/shared"
)

func (e *Evaluator) eval_instruction(expr shared.Node) (shared.Node, error) {
	var err error
	instruction_type := expr.Children[0].Val.Value
	instruction_args := expr.Children[1].Children

	switch instruction_type {
	case lexer.II_set:
		set_ref := instruction_args[0]
		var set_values []shared.Node
		set_values, err = e.eval_children(instruction_args[1:])
		if err != nil {
			return shared.Node{}, err
		}

		if set_ref.Val.Type != shared.TTref {
			err = &EvalError{message: "set can only be used on tape", pos: expr.Val.Pos}
			return shared.Node{}, err
		}

		e.setRefValue(
			set_ref.Val.Value, set_values[0].Val.Value)

		return set_ref, err

	case lexer.II_print:
		var args []shared.Node
		args, err = e.eval_children(instruction_args)
		if err != nil {
			return shared.Node{}, err
		}
		printString := args[0].Val.Value
		printString = e.unrefString(printString)
		fmt.Println(printString)

	case lexer.II_m:
		mathInput := instruction_args[0].Val.Value
		mathInput = e.unrefString(mathInput)
		mResult, err := m.Do(mathInput)
		if err != nil {
			err = &MathEvalError{message: "expression: \"" + mathInput + "\"produced error: " + err.Error(), pos: expr.Val.Pos}
		}
		return makeNumberNode(mResult), err

	case lexer.II_not:
		var args []shared.Node
		args, err = e.eval_children(instruction_args)
		if err != nil {
			return shared.Node{}, err
		}
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
		return instruction_args[0], err

	case lexer.II_smile:
		smiles := []string{"〈( ^.^)ノ", "(>̯-̮<̯)", "(/^▽^)/", "(⌐■_■)", "( ¬_¬)", "( o.O)"}

		return makeStringNode(smiles[rand.Int()%len(smiles)]), err

	case lexer.II_inz:
		var args []shared.Node
		args, err = e.eval_children(instruction_args)
		if err != nil {
			return shared.Node{}, err
		}
		if !stringIsTrue(args[0].Val.Value) {
			break
		}

		split := splitComments(args[1].Val.Value)

		var pos int
		pos, err = strconv.Atoi(args[2].Val.Value)
		if err != nil {
			// TODO: replace with propper error
			return shared.Node{}, err
		}

		// pass by reference hell
		out := make([]string, len(split))
		copy(out, split)
		out = append(
			out[:pos],
			args[3].Val.Value)
		out = append(out, split[pos:]...)

		return makeStringNode(strings.Join(out, ",")), err
	case lexer.II_dnz:
		var args []shared.Node
		args, err = e.eval_children(instruction_args)
		if err != nil {
			return shared.Node{}, err
		}
		if !stringIsTrue(args[0].Val.Value) {
			break
		}

		split := splitComments(args[1].Val.Value)

		var pos int
		pos, err = strconv.Atoi(args[2].Val.Value)
		if err != nil {
			// TODO: replace with propper error
			return shared.Node{}, err
		}

		if pos >= len(split)-1 {
			break
		}

		split = append(split[:pos], split[pos+1:]...)

		return makeStringNode(strings.Join(split, ",")), err
	case lexer.II_peek:
		args, err := e.eval_children(instruction_args)

		arr := strings.Split(args[0].Val.Value, "\n")

		index, err := strconv.Atoi(args[1].Val.Value)
		if err != nil {
			log.Fatal((&TypeError{"expected int", args[1].Val.Pos}).Error())
		}

		if index > len(arr) {
			log.Fatal((&OutOfRangeError{index, len(arr), args[1].Val.Pos}).Error())
		}

		return makeStringNode(arr[index]), err
	case lexer.II_place:
		args, err := e.eval_children(instruction_args)

		arr := strings.Split(args[0].Val.Value, "\n")

		index, err := strconv.Atoi(args[1].Val.Value)
		if err != nil {
			log.Fatal((&TypeError{"expected int", args[1].Val.Pos}).Error())
		}

		if index > len(arr) {
			log.Fatal((&OutOfRangeError{index, len(arr), args[1].Val.Pos}).Error())
		}

		arr[index] = args[2].Val.Value

		return makeStringNode(strings.Join(arr, "\n")), err
	case lexer.II_grow:
		args, err := e.eval_children(instruction_args)

		arr := strings.Split(args[0].Val.Value, "\n")

		toAdd, err := strconv.Atoi(args[1].Val.Value)
		if err != nil {
			log.Fatal((&TypeError{"expected int", args[1].Val.Pos}).Error())
		}

		arr = append(arr, make([]string, toAdd)...)

		for i := len(arr) - toAdd; i < len(arr); i++ {
			arr[i] = "0"
		}

		return makeStringNode(strings.Join(arr, "\n")), err

	case lexer.II_len:
		args, err := e.eval_children(instruction_args)
		arr := strings.Split(args[0].Val.Value, "\n")

		return makeNumberNode(strconv.Itoa(len(arr))), err
		// TODO: add default which throws error
	}
	return shared.Node{}, err
}
