package parser

import (
	"fmt"

	"github.com/grossamos/jam0001/shared"
)

type ParseBracketError struct {
	pos shared.Position
}

func (pbe *ParseBracketError) Error() string {
	return fmt.Sprintf("ParseBracketError (at line %d)", pbe.pos.Line)
}

type MissingBlockError struct {
	pos shared.Position
}

func (mbe *MissingBlockError) Error() string {
	return fmt.Sprintf("MissingBlockError (at line %d)", mbe.pos.Line)
}

type UnexpectedError struct {
	value string
	pos   shared.Position
}

func (ue *UnexpectedError) Error() string {
	return fmt.Sprintf("UnexpectedError: Unexpected %s (at line %d)", ue.value, ue.pos)
}

type IncorrectSignatureError struct {
	message string
	pos     shared.Position
}

func (ise *IncorrectSignatureError) Error() string {
	return fmt.Sprintf("IncorrectSignatureError: Incorrect %s signature (at line %d)", ise.message, ise.pos)
}
