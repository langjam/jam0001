package lexer

const comment_letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_- \t"

func (l *Lexer) make_text() string {
	text_str := ""
	l.advance()
	for l.current_char != '"' {
		text_str += string(l.current_char)
		l.advance()
	}
	return text_str
}
