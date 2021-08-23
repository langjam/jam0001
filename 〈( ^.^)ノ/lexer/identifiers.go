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
const II_smile = "smile"
const II_break = "break"
const II_inz = "inz"
const II_dnz = "dnz"
const II_place = "place"
const II_peek = "peek"
const II_grow = "grow"
const II_len = "len"

const instruction_size_min = 3
const instruction_size_max = 5

func (l *Lexer) makeIdentifierToken() (shared.Token, error) {
	if l.currentChar == 'm' {
		l.advance()
		return shared.Token{Type: shared.TTinstruction, Value: II_m}, nil
	}

	inst := make([]rune, instruction_size_min)

	for i := 0; i < instruction_size_min; i++ {
		inst[i] = l.currentChar
		l.advance()
	}

	if string(inst) == II_set {
		return shared.Token{Type: shared.TTinstruction, Value: II_set, Pos: l.pos}, nil
	} else if string(inst) == II_and {
		return shared.Token{Type: shared.TTinstruction, Value: II_and, Pos: l.pos}, nil
	} else if string(inst) == II_not {
		return shared.Token{Type: shared.TTinstruction, Value: II_not, Pos: l.pos}, nil
	} else if string(inst) == II_dnz {
		return shared.Token{Type: shared.TTinstruction, Value: II_dnz, Pos: l.pos}, nil
	} else if string(inst) == II_inz {
		return shared.Token{Type: shared.TTinstruction, Value: II_inz, Pos: l.pos}, nil
	} else if string(inst) == II_len {
		return shared.Token{Type: shared.TTinstruction, Value: II_len, Pos: l.pos}, nil
	}

	inst = append(inst, l.currentChar)
	l.advance()
	if string(inst) == II_true {
		return shared.Token{Type: shared.TTconst, Value: II_true, Pos: l.pos}, nil
	} else if string(inst) == II_peek {
		return shared.Token{Type: shared.TTinstruction, Value: II_peek, Pos: l.pos}, nil
	} else if string(inst) == II_grow {
		return shared.Token{Type: shared.TTinstruction, Value: II_grow, Pos: l.pos}, nil
	}

	for i := len(inst); i < instruction_size_max; i++ {
		inst = append(inst, l.currentChar)
		l.advance()
	}

	if string(inst) == II_print {
		return shared.Token{Type: shared.TTinstruction, Value: II_print, Pos: l.pos}, nil
	} else if string(inst) == II_while {
		return shared.Token{Type: shared.TTwhile, Value: II_while, Pos: l.pos}, nil
	} else if string(inst) == II_false {
		return shared.Token{Type: shared.TTconst, Value: II_false, Pos: l.pos}, nil
	} else if string(inst) == II_smile {
		return shared.Token{Type: shared.TTinstruction, Value: II_smile, Pos: l.pos}, nil
	} else if string(inst) == II_break {
		return shared.Token{Type: shared.TTinstruction, Value: II_break, Pos: l.pos}, nil
	} else if string(inst) == II_place {
		return shared.Token{Type: shared.TTinstruction, Value: II_place, Pos: l.pos}, nil
	}

	return shared.Token{}, &IllegalInstructionError{string(inst), l.pos}
}
