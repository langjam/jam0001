package lexer

import (
	"strings"

	"github.com/grossamos/jam0001/shared"
)

func (l *Lexer) makeRef() (shared.Token, error) {
	refNumString := ""
	l.advance()
	if l.currentChar == '-' {
		refNumString += "-"
		l.advance()
	}
	for strings.ContainsRune(digitChars, l.currentChar) {
		refNumString += string(l.currentChar)
		l.advance()
	}
	return shared.Token{Type: shared.TTref, Value: refNumString, Pos: l.pos}, nil
}
