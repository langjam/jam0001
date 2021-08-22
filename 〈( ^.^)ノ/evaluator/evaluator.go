package evaluator

import (
	"fmt"
	"os"
	"strconv"

	"github.com/grossamos/jam0001/evaluator/m"
	"github.com/grossamos/jam0001/lexer"
	"github.com/grossamos/jam0001/shared"
)

// some eval
type Evaluator struct {
	Nodes    []shared.Node

	negative []string
	positive []string
	zero     string

	comments []string
}

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
		fmt.Println(args[0].Val.Value)

	case lexer.II_m:
		mathInput := instruction_args[0].Val.Value
		mResult, err := m.Do(mathInput, e.negative, e.positive)
		if err != nil {
			fmt.Println("MathError: expression invalid: ", mathInput)
		}
		return makeNumberNode(mResult)
	}
	return shared.Node{}
}

func (e *Evaluator) eval_string_call(expr shared.Node) shared.Node {
	// TODO translating strings

	return shared.Node{}
}

func (e *Evaluator) eval_while(expr shared.Node) shared.Node {
	for isTrue(e.eval_expr(expr.Children[1]).Val.Value) {
		e.eval_expr(expr.Children[2])
		break
	}

	return shared.Node{}
}

func (e *Evaluator) eval_expr(expr shared.Node) shared.Node {
	if !expr.IsExpression {
		if expr.Val.Type == shared.TTref { // get reference
			expr.Val.Type = shared.TTstring
			expr.Val.Value = e.getRefValue(expr.Val.Value)
		}

		return expr
	}

	for i:=0; i < len(expr.Children); i++ { // for all children
		// if it is an expression, recursively evaluate it
		if expr.Children[i].IsExpression { // this fucking code makes the destruction
			expr.Children[i] = e.eval_expr(expr.Children[i])
			return e.eval_expr(expr)
		}

		switch expr.Children[i].Val.Type {
		case shared.TTinstruction:
			return e.eval_instruction(expr)
  
		case shared.TTstring:
			return e.eval_string_call(expr)
  
		case shared.TTnumber, shared.TTconst, shared.TTref:
			return expr.Children[i]
  
		case shared.TTwhile:
			return e.eval_while(expr)
  
		case shared.TTnull:
  
		case shared.TTwcomment, shared.TTwcommentAdd: // skip comments
  
		default:
			fmt.Println("Unimplemented feature:", expr.Val)
			os.Exit(1)
			return shared.Node{}
		}
	}

	return shared.Node{}
}

func isTrue(val string) bool {
	return val != "0" && val != "FALSE" && val != ""
}

func isInstruction(possible_instruction shared.Node) bool {
	return possible_instruction.Val.Type == shared.TTinstruction
}

func (e *Evaluator) eval_children(parentNode []shared.Node) []shared.Node {
	out := make([]shared.Node, len(parentNode))

	for index, subnode := range parentNode {
		out[index] = e.eval_expr(subnode)
	}

	return out
}

func (e *Evaluator) getRefValue(ref string) string {
	varIndex, err := strconv.Atoi(ref)
	if err != nil {
		fmt.Println("Incorrect variable reference.")
		os.Exit(1)
	}

	if varIndex == 0 {
		return e.zero
	}

	arr := e.positive
	if varIndex < 0 {
		arr = e.negative
		varIndex *= -1
	}

	if len(arr) < varIndex {
		fmt.Println("Variable ref", ref, ": out of range.")
		os.Exit(1)
	}

	return arr[varIndex]
}

func (e *Evaluator) setRefValue(ref, value string) {
	varIndex, err := strconv.Atoi(ref)
	if err != nil {
		fmt.Println("Incorrect variable reference.")
		os.Exit(1)
	}

	if varIndex == 0 {
		e.zero = value
	}

	arr := e.positive
	if varIndex < 0 {
		arr = e.negative
		varIndex *= -1
	}

	if len(arr) < varIndex {
		fmt.Println("Variable set: out of range.")
		os.Exit(1)
	}

	arr[varIndex] = value
}

func makeNumberNode(number string) shared.Node {
	return shared.Node{Val: shared.Token{
		Type:  shared.TTnumber,
		Value: number},
		IsExpression: false, Children: []shared.Node{}}
}

func RunEvaluator(nodes []shared.Node) {
	evaluator := Evaluator{nodes, []string{"0", "0", "0", "0"}, []string{}, "0", []string{}}
	for _, node := range nodes {
		evaluator.eval_expr(node)
	}
}
