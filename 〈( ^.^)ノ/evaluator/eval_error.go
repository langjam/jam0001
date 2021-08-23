package evaluator

import (
	"fmt"

	"github.com/grossamos/jam0001/shared"
)

type MissingArgumentError struct {
	message string
	pos     shared.Position
}

func (mae *MissingArgumentError) Error() string {
	return fmt.Sprintf("MissingArgumentError (at line %d): %s", mae.pos.Line, mae.message)
}

type TypeError struct {
	message string
	pos     shared.Position
}

func (te *TypeError) Error() string {
	return fmt.Sprintf("TypeError (at line %d): %s", te.pos.Line, te.message)
}

type MathEvalError struct {
	message string
	pos     shared.Position
}

func (err *MathEvalError) Error() string {
	return fmt.Sprintf("MathEvalError (at line %d): %s", err.pos.Line, err.message)
}

type EvalError struct {
	message string
	pos     shared.Position
}

func (err *EvalError) Error() string {
	return fmt.Sprintf("EvalError (at line %d): %s", err.pos.Line, err.message)
}

type UnimplementedError struct {
	message string
	pos     shared.Position
}

func (err *UnimplementedError) Error() string {
	return fmt.Sprintf("UnimplementedError (at line %d): %s", err.pos.Line, err.message)
}

type OutOfRangeError struct {
	index, r int
	pos shared.Position
}

func (err *OutOfRangeError) Error() string {
	return fmt.Sprintf("OutOfRangeError (at line %d): %d > %d", err.pos.Line, err.index, err.r)
}
