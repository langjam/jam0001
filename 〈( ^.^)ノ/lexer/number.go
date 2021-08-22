package lexer

import (
	"strings"

	"github.com/grossamos/jam0001/shared"
)

const digitChars string = "0123456789"

func (l *Lexer) makeNumber() (shared.Token, error) {
	numberString := ""
	dotCount := 0
	for l.currentChar != '\x00' && (l.currentChar == '.' || strings.ContainsRune(digitChars, l.currentChar)) {
		if l.currentChar == '.' {
			if len(numberString) == 0 {
				return shared.Token{}, &IllegalNumberFormatError{message: ".", pos: l.pos}
			}
			if dotCount >= 1 {
				break
			}
			dotCount += 1
			numberString += string(l.currentChar)
		} else {
			numberString += string(l.currentChar)
		}
		l.advance()
	}
	return shared.Token{Type: shared.TTnumber, Value: numberString, Pos: l.pos}, nil
}
