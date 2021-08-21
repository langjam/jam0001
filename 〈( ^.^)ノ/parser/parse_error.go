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
