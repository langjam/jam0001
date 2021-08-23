package lexer

import (
	"log"
	"strings"

	"github.com/grossamos/jam0001/shared"
)

type Lexer struct {
	text        string
	index       int
	pos         shared.Position
	currentChar rune
}

func NewLexer(text string) (lex Lexer) {
	lex = Lexer{text, -1, shared.Position{Line: 0}, '\x00'}
	lex.advance()
	return
}

func (l *Lexer) advance() {
	l.index += 1

	if len(l.text) > l.index {
		l.currentChar = ([]rune(l.text))[l.index]
	} else {
		l.currentChar = '\x00'
	}

	if l.currentChar == '\n' {
		l.pos.Advance()
	}
}

func (l *Lexer) makeTokens() ([]shared.Token, error) {
	var tokens []shared.Token
	for l.currentChar != '\x00' {
		var err error
		// handle identifiers
		if l.currentChar == 's' || l.currentChar == 'm' || l.currentChar == 'a' ||
			l.currentChar == 'n' || l.currentChar == 'w' || l.currentChar == 'T' ||
			l.currentChar == 'F' || l.currentChar == 'p' || l.currentChar == 'i' ||
			l.currentChar == 'd' || l.currentChar == 'g' || l.currentChar == 'l' ||
			l.currentChar == 'b' {
			var instToken shared.Token
			instToken, err = l.makeIdentifierToken()
			tokens = append(tokens, instToken)
		} else if l.currentChar == '(' || l.currentChar == ')' || l.currentChar == '{' || l.currentChar == '}' {
			var charToken shared.Token
			charToken, err = l.makeParenToken()
			tokens = append(tokens, charToken)
			l.advance()
		} else if l.currentChar == ' ' || l.currentChar == '\n' || l.currentChar == '\t' {
			l.advance()
		} else if l.currentChar == '.' || strings.ContainsRune(digitChars, l.currentChar) {
			var numToken shared.Token
			numToken, err = l.makeNumber()
			tokens = append(tokens, numToken)
		} else if l.currentChar == '$' {
			var refToken shared.Token
			refToken, err = l.makeRef()
			tokens = append(tokens, refToken)
		} else if l.currentChar == '"' {
			tokens = append(tokens, shared.Token{Type: shared.TTstring, Value: l.make_text()})
			if l.currentChar != '"' {
				return []shared.Token{}, &IllegalSyntaxError{message: "Error finding closed bracket" + string(l.currentChar), pos: l.pos}
			}
			l.advance()
		} else if l.currentChar == '/' {
			// ignore comments
			if l.index == 0 || l.text[l.index-1] == '\n' {
				for l.currentChar != '\n' {
					l.advance()
				}
				continue
			}
			l.advance()
			if l.currentChar != '/' {
				return []shared.Token{}, &IllegalSyntaxError{message: "Invalid comment" + string(l.currentChar), pos: l.pos}
			}
			l.advance()
			tokens = append(tokens, shared.Token{Type: shared.TTwcomment, Value: l.make_text()})
			l.advance()
		} else {
			return []shared.Token{}, &IllegalSyntaxError{message: "invalid identifier \"" + string(l.currentChar) + "\"", pos: l.pos}
		}

		if err != nil {
			return []shared.Token{}, err
		}
	}
	return tokens, nil
}

func RunLexer(text string) []shared.Token {
	lex := NewLexer(text)
	tokens, err := lex.makeTokens()
	if err != nil {
		log.Fatal(err)
	}
	return tokens
}
