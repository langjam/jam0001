package lexer

import "github.com/grossamos/jam0001/shared"

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

func (l *Lexer) make_tokens() []shared.Token {
	var tokens []shared.Token
	tokens = append(tokens, shared.Token{Type: shared.TTnull, Value: l.text, Pos: l.pos})
	return tokens
}

func RunLexer(text string) []shared.Token {
	lex := NewLexer(text)
	tokens := lex.make_tokens()
	return tokens
}
