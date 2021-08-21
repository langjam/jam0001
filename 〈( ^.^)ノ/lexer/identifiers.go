package lexer

import "github.com/grossamos/jam0001/shared"

const ii_m = "m"
const ii_set = "set"
const ii_and = "and"
const ii_not = "not"
const ii_print = "print"
const ii_while = "while"
const ii_true = "TRUE"
const ii_false = "FALSE"

const instruction_size_min = 3
const instruction_size_max = 5

func (l *Lexer) make_identifier_token() (shared.Token, error) {
	if l.current_char == 'm' {
		return shared.Token{Type: shared.TTinstruction, Value: ii_m}, nil
	}

	inst := make([]rune, instruction_size_min)

	for i := 0; i < instruction_size_min; i++ {
		inst[i] = l.current_char
		l.advance()
	}

	if string(inst) == ii_set {
		return shared.Token{Type: shared.TTinstruction, Value: ii_set, Pos: l.pos}, nil
	} else if string(inst) == ii_and {
		return shared.Token{Type: shared.TTinstruction, Value: ii_and, Pos: l.pos}, nil
	} else if string(inst) == ii_not {
		return shared.Token{Type: shared.TTinstruction, Value: ii_not, Pos: l.pos}, nil
	}

	for i := instruction_size_min; i < instruction_size_max; i++ {
		inst = append(inst, l.current_char)
		l.advance()
	}

	if string(inst) == ii_print {
		return shared.Token{Type: shared.TTinstruction, Value: ii_print, Pos: l.pos}, nil
	} else if string(inst) == ii_while {
		return shared.Token{Type: shared.TTwhile, Value: ii_while, Pos: l.pos}, nil
	} else if string(inst) == ii_false {
		return shared.Token{Type: shared.TTconst, Value: ii_false, Pos: l.pos}, nil
	} else if string(inst)[:4] == ii_true {
		return shared.Token{Type: shared.TTconst, Value: ii_true, Pos: l.pos}, nil
	}

	return shared.Token{}, &IllegalInstructionError{string(inst), l.pos}
}
