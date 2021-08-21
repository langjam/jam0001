package lexer

import (
	"strings"

	"github.com/grossamos/jam0001/shared"
)

func (l *Lexer) make_ref() (shared.Token, error) {
	ref_num_str := ""
	l.advance()
	if l.current_char == '-' {
		ref_num_str += "-"
		l.advance()
	}
	for strings.ContainsRune(digitChars, l.current_char) {
		ref_num_str += string(l.current_char)
		l.advance()
	}
	return shared.Token{Type: shared.TTref, Value: ref_num_str, Pos: l.pos}, nil
}
