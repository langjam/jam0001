package lexer

import "github.com/grossamos/jam0001/shared"

const II_m = "m"
const II_set = "set"
const II_and = "and"
const II_not = "not"
const II_print = "print"
const II_while = "while"
const II_true = "TRUE"
const II_false = "FALSE"

const instruction_size_min = 3
const instruction_size_max = 5

func (l *Lexer) make_identifier_token() (shared.Token, error) {
	if l.current_char == 'm' {
		return shared.Token{Type: shared.TTinstruction, Value: II_m}, nil
	}

	inst := make([]rune, instruction_size_min)

	for i := 0; i < instruction_size_min; i++ {
		inst[i] = l.current_char
		l.advance()
	}

	if string(inst) == II_set {
		return shared.Token{Type: shared.TTinstruction, Value: II_set, Pos: l.pos}, nil
	} else if string(inst) == II_and {
		return shared.Token{Type: shared.TTinstruction, Value: II_and, Pos: l.pos}, nil
	} else if string(inst) == II_not {
		return shared.Token{Type: shared.TTinstruction, Value: II_not, Pos: l.pos}, nil
	}

	for i := instruction_size_min; i < instruction_size_max; i++ {
		inst = append(inst, l.current_char)
		l.advance()
	}

	if string(inst) == II_print {
		return shared.Token{Type: shared.TTinstruction, Value: II_print, Pos: l.pos}, nil
	} else if string(inst) == II_while {
		return shared.Token{Type: shared.TTwhile, Value: II_while, Pos: l.pos}, nil
	} else if string(inst) == II_false {
		return shared.Token{Type: shared.TTconst, Value: II_false, Pos: l.pos}, nil
	} else if string(inst)[:4] == II_true {
		return shared.Token{Type: shared.TTconst, Value: II_true, Pos: l.pos}, nil
	}

	return shared.Token{}, &IllegalInstructionError{string(inst), l.pos}
}
