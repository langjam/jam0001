package lexer

import (
	"fmt"
	"strings"

	"github.com/grossamos/jam0001/shared"
)

type Lexer struct {
	text         string
	index        int
	pos          shared.Position
	current_char rune
}

func NewLexer(text string) (lex Lexer) {
	lex = Lexer{text, -1, shared.Position{Line: 0}, '\x00'}
	lex.advance()
	return
}

func (l *Lexer) advance() {
	l.index += 1

	if len(l.text) > l.index {
		l.current_char = ([]rune(l.text))[l.index]
	} else {
		l.current_char = '\x00'
	}

	if l.current_char == '\n' {
		l.pos.Advance()
	}
}

func (l *Lexer) make_tokens() ([]shared.Token, error) {
	var tokens []shared.Token
	for l.current_char != '\x00' {
		var err error
		// handle identifiers
		if l.current_char == 's' || l.current_char == 'm' || l.current_char == 'a' || l.current_char == 'n' || l.current_char == 'w' || l.current_char == 'T' || l.current_char == 'F' {
			var inst_token shared.Token
			inst_token, err = l.make_identifier_token()
			tokens = append(tokens, inst_token)
		} else if l.current_char == '(' || l.current_char == ')' || l.current_char == '{' || l.current_char == '}' {
			var char_token shared.Token
			char_token, err = l.make_paren_token()
			tokens = append(tokens, char_token)
			l.advance()
		} else if l.current_char == ' ' || l.current_char == '\n' || l.current_char == '\t' {
			l.advance()
		} else if l.current_char == '.' || strings.ContainsRune(digitChars, l.current_char) {
			var num_token shared.Token
			num_token, err = l.make_number()
			tokens = append(tokens, num_token)
		} else if l.current_char == '$' {
			var ref_token shared.Token
			ref_token, err = l.make_ref()
			tokens = append(tokens, ref_token)
		} else if l.current_char == '"' {
			// l.advance()
			tokens = append(tokens, shared.Token{Type: shared.TTstring, Value: l.make_text()})
			if l.current_char != '"' {
				return []shared.Token{}, &IllegalSyntaxError{message: "Error finding closed bracket" + string(l.current_char), pos: l.pos}
			}
			l.advance()
		} else if l.current_char == '/' {
			l.advance()
			if l.current_char != '/' {
				return []shared.Token{}, &IllegalSyntaxError{message: "Invalid comment" + string(l.current_char), pos: l.pos}
			}
			l.advance()
			tokens = append(tokens, shared.Token{Type: shared.TTwcomment, Value: l.make_text()})
			l.advance()
		} else {
			return []shared.Token{}, &IllegalSyntaxError{message: "invalid identifier" + string(l.current_char), pos: l.pos}
		}

		if err != nil {
			return []shared.Token{}, err
		}
		// l.advance()
	}
	return tokens, nil
}

// func (l *Lexer) peak_ahead(indecies int) string {
// 	return l.text[l.index : l.index+indecies]
// }

func RunLexer(text string) []shared.Token {
	lex := NewLexer(text)
	tokens, err := lex.make_tokens()
	if err != nil {
		fmt.Println(err.Error())
	}
	return tokens
}
