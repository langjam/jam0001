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
