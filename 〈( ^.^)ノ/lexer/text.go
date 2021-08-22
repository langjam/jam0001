package lexer

func (l *Lexer) make_text() string {
	textString := ""
	l.advance()
	for l.currentChar != '"' {
		textString += string(l.currentChar)
		l.advance()
	}
	return textString
}
