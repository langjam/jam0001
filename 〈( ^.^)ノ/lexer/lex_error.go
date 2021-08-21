package lexer

import (
	"fmt"

	"github.com/grossamos/jam0001/shared"
)

type IllegalInstructionError struct {
	Instruction string
	pos         shared.Position
}

func (ie *IllegalInstructionError) Error() string {
	return fmt.Sprintf("IllegalInstructionError (at line %d): %s", ie.pos.Line, ie.Instruction)
}

type IllegalCharError struct {
	message string
	pos     shared.Position
}

func (ce *IllegalCharError) Error() string {
	return fmt.Sprintf("IllegalCharError (at line %d): %s", ce.pos.Line, ce.message)
}

type IllegalSyntaxError struct {
	message string
	pos     shared.Position
}

func (se *IllegalSyntaxError) Error() string {
	return fmt.Sprintf("IllegalSyntaxError (at line %d): %s", se.pos.Line, se.message)
}

type IllegalNumberFormatError struct {
	message string
	pos     shared.Position
}

func (ne *IllegalNumberFormatError) Error() string {
	return fmt.Sprintf("IllegalNumberFormatError (at line %d): %s", ne.pos.Line, ne.message)
}
