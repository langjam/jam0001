package lexer

import (
	"strings"

	"github.com/grossamos/jam0001/shared"
)

const digitChars string = "0123456789"

func (l *Lexer) make_number() (shared.Token, error) {
	number_str := ""
	dot_count := 0
	for l.current_char != '\x00' && (l.current_char == '.' || strings.ContainsRune(digitChars, l.current_char)) {
		if l.current_char == '.' {
			if len(number_str) == 0 {
				return shared.Token{}, &IllegalNumberFormatError{message: ".", pos: l.pos}
			}
			if dot_count >= 1 {
				break
			}
			dot_count += 1
			number_str += string(l.current_char)
		} else {
			number_str += string(l.current_char)
		}
		l.advance()
	}
	return shared.Token{Type: shared.TTnumber, Value: number_str, Pos: l.pos}, nil
}
