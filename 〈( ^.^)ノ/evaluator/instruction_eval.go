package evaluator

import (
	"fmt"
	"math/rand"
	"strconv"
	"strings"
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
		smiles := []string{"〈( ^.^)ノ", "(>̯-̮<̯)", "(/^▽^)/", "(⌐■_■)", "( ¬_¬)", "(O.o )"}

		return makeStringNode(smiles[rand.Int()%len(smiles)])

	case lexer.II_inz:
		args := e.eval_children(instruction_args)
		if !stringIsTrue(args[0].Val.Value) {
			break
		}

		split := splitComments(args[1].Val.Value)	

		pos, err := strconv.Atoi(args[2].Val.Value)
		if err != nil {
			log.Fatal((&TypeError{"expected int", args[2].Val.Pos}).Error())
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
			log.Fatal((&TypeError{"expected int", args[2].Val.Pos}).Error())
		}

		if pos >= len(split) -1 {
			break
		}

		split = append(split[:pos], split[pos+1:]...)

		return makeStringNode(strings.Join(split, ","))
	case lexer.II_peek:
		args := e.eval_children(instruction_args)

		arr := strings.Split(args[0].Val.Value, "\n")

		index, err := strconv.Atoi(args[1].Val.Value)
		if err != nil {
			log.Fatal((&TypeError{"expected int", args[1].Val.Pos}).Error())
		}

		if index > len(arr) {
			log.Fatal((&OutOfRangeError{index, len(arr), args[1].Val.Pos}).Error())
		}

		return makeStringNode(arr[index])
	case lexer.II_place:
		args := e.eval_children(instruction_args)

		arr := strings.Split(args[0].Val.Value, "\n")

		index, err := strconv.Atoi(args[1].Val.Value)
		if err != nil {
			log.Fatal((&TypeError{"expected int", args[1].Val.Pos}).Error())
		}

		if index > len(arr) {
			log.Fatal((&OutOfRangeError{index, len(arr), args[1].Val.Pos}).Error())
		}

		arr[index] = args[2].Val.Value

		return makeStringNode(strings.Join(arr, "\n"))
	case lexer.II_grow:
		args := e.eval_children(instruction_args)

		arr := strings.Split(args[0].Val.Value, "\n")

		toAdd, err := strconv.Atoi(args[1].Val.Value)
		if err != nil {
			log.Fatal((&TypeError{"expected int", args[1].Val.Pos}).Error())
		}

		arr = append(arr, make([]string, toAdd)...)

		for i:=len(arr)-toAdd; i < len(arr); i++ {
			arr[i] = "0"
		}

		return makeStringNode(strings.Join(arr, "\n"))

	case lexer.II_len:
		args := e.eval_children(instruction_args)
		arr := strings.Split(args[0].Val.Value, "\n")

		return makeNumberNode(strconv.Itoa(len(arr)))
	
	}
	return shared.Node{}
}
