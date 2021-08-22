package m

import (
	"errors"
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"strings"
)

//============================================================================
// Language Constants
//============================================================================

const (
	Sentinal = iota
	NumberLiteral
	NegativeCellReference
	PositiveCellReference
	OpenParen
	CloseParen
	AddOp
	SubOp
	MulOp
	DivOp
	ExpOp
	LTOp
	LEOp
	GTOp
	GEOp
	EqOp
	NEqOp
	FirstNamedFunction
	AbsOp
	SqrtOp
	SinOp
	CosOp
	TanOp
	RandOp
	LastNamedFunction
)

var PrecedenceMap map[int]int = map[int]int{
	Sentinal:   -1,
	OpenParen:  0,
	CloseParen: 0,
	AddOp:      2,
	SubOp:      2,
	MulOp:      3,
	DivOp:      3,
	ExpOp:      4,
	LTOp:       1,
	LEOp:       1,
	GTOp:       1,
	GEOp:       1,
	EqOp:       1,
	NEqOp:      1,
	AbsOp:      10,
	SqrtOp:     10,
	SinOp:      10,
	CosOp:      10,
	TanOp:      10,
	RandOp:     10,
}

//============================================================================
// Text matching functions
//============================================================================

// Returns true if x shows up in chars
func MatchAny(x byte, chars string) bool {
	for _, v := range chars {
		if x == byte(v) {
			return true
		}
	}
	return false
}

// Returns true if x shows up in ranges, e.g. "09az" would be true if x is in 0..9 || a..z
func MatchRanges(x byte, ranges string) bool {
	for len(ranges) != 0 {
		if len(ranges) == 1 {
			return false
		}
		if x >= ranges[0] && x <= ranges[1] {
			return true
		}
		ranges = ranges[2:]
	}
	return false
}

//============================================================================
// Lexer
//============================================================================

type Token struct {
	kind  int
	value string
}

type Lexer struct {
	in        string  // source text
	matchSize int     // number of currently matched bytes
	out       []Token // output token stream
}

//============================================================================

// Grabs everything after the matched section
func (l *Lexer) Suffix() string {
	if l.matchSize != 0 {
		return l.in[l.matchSize:]
	}
	return l.in
}

// Asserts that we've matched more bytes
func (l *Lexer) AccumulateMatch(size int) bool {
	l.matchSize += size
	return size != 0
}

// Consumes the matched bytes
func (l *Lexer) Eat() bool {
	if l.matchSize == 0 {
		return false
	}
	l.in = l.Suffix()
	l.matchSize = 0
	return true
}

func (l *Lexer) MatchPrefix(prefix string) bool {
	if strings.HasPrefix(l.Suffix(), prefix) {
		return l.AccumulateMatch(len(prefix))
	}
	return false
}

func (l *Lexer) MatchAny(ranges string) bool {
	in := l.Suffix()
	count := 0
	for count < len(in) && MatchAny(in[count], ranges) {
		count++
	}
	return l.AccumulateMatch(count)
}

func (l *Lexer) MatchRanges(ranges string) bool {
	in := l.Suffix()
	count := 0
	for count < len(in) && MatchRanges(in[count], ranges) {
		count++
	}
	return l.AccumulateMatch(count)
}

//============================================================================

func (l *Lexer) EatPushToken(kind int) bool {
	if l.matchSize == 0 {
		return false
	}
	l.out = append(l.out, Token{kind, l.in[:l.matchSize]})
	return l.Eat()
}

func (l *Lexer) Lex(input string) error {
	l.in = input
	l.out = []Token{}

	for len(l.in) != 0 {
		if l.MatchAny(" ") {
			l.Eat()
		} else if l.MatchPrefix("(") {
			l.EatPushToken(OpenParen)
		} else if l.MatchPrefix(")") {
			l.EatPushToken(CloseParen)
		} else if l.MatchPrefix("+") {
			l.EatPushToken(AddOp)
		} else if l.MatchPrefix("-") {
			l.EatPushToken(SubOp)
		} else if l.MatchPrefix("*") {
			l.EatPushToken(MulOp)
		} else if l.MatchPrefix("/") {
			l.EatPushToken(DivOp)
		} else if l.MatchPrefix("^") {
			l.EatPushToken(ExpOp)
		} else if l.MatchPrefix("==") {
			l.EatPushToken(EqOp)
		} else if l.MatchPrefix("!=") {
			l.EatPushToken(NEqOp)
		} else if l.MatchPrefix("<=") {
			l.EatPushToken(LEOp)
		} else if l.MatchPrefix(">=") {
			l.EatPushToken(GEOp)
		} else if l.MatchPrefix("<") {
			l.EatPushToken(LTOp)
		} else if l.MatchPrefix(">") {
			l.EatPushToken(GTOp)
		} else if l.MatchPrefix("abs") {
			l.EatPushToken(AbsOp)
		} else if l.MatchPrefix("sqrt") {
			l.EatPushToken(SqrtOp)
		} else if l.MatchPrefix("sin") {
			l.EatPushToken(SinOp)
		} else if l.MatchPrefix("cos") {
			l.EatPushToken(CosOp)
		} else if l.MatchPrefix("tan") {
			l.EatPushToken(TanOp)
		} else if l.MatchPrefix("rand") {
			l.EatPushToken(RandOp)
		} else if l.MatchRanges("09") {
			l.EatPushToken(NumberLiteral)
		} else if l.MatchPrefix("$") {
			kind := PositiveCellReference
			if l.MatchPrefix("-") {
				kind = NegativeCellReference
			}
			l.Eat()
			if !l.MatchRanges("09") {
				return errors.New("unrecognized byte while lexing expression")
			}
			l.EatPushToken(kind)
		} else {
			return errors.New("unrecognized byte while lexing expression")
		}
	}
	return nil
}

//============================================================================
// Evaluator
//============================================================================

type Operation struct {
	kind       int
	precedence int
}

type Evaluator struct {
	values     []int       // A stack of values used during evaluation
	operations []Operation // a stack of deferred operations used implement precedence
}

// Pushes a value to the value stack
func (e *Evaluator) PushValue(text string) error {
	value, err := strconv.Atoi(text)
	if err != nil {
		return errors.New("failed to parse int '" + text + "'")
	}
	e.values = append(e.values, value)
	return nil
}

// Executes the operator at the top of the operator stack.
func (e *Evaluator) Execute() error {
	switch e.operations[len(e.operations)-1].kind {
	case AddOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for +")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		e.values = append(e.values[:length-2], a+b)
	case SubOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for -")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		e.values = append(e.values[:length-2], a-b)
	case MulOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for *")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		e.values = append(e.values[:length-2], a*b)
	case DivOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for /")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		e.values = append(e.values[:length-2], a/b)
	case ExpOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for ^")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		e.values = append(e.values[:length-2], int(math.Pow(float64(a), float64(b))))
	case LTOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for <")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		result := 0
		if a < b {
			result = 1
		}
		e.values = append(e.values[:length-2], result)
	case LEOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for <=")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		result := 0
		if a <= b {
			result = 1
		}
		e.values = append(e.values[:length-2], result)
	case GTOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for >")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		result := 0
		if a > b {
			result = 1
		}
		e.values = append(e.values[:length-2], result)
	case GEOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for >=")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		result := 0
		if a >= b {
			result = 1
		}
		e.values = append(e.values[:length-2], result)
	case EqOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for ==")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		result := 0
		if a == b {
			result = 1
		}
		e.values = append(e.values[:length-2], result)
	case NEqOp:
		length := len(e.values)
		if length < 2 {
			return errors.New("too few arguments for !=")
		}
		a := e.values[length-2]
		b := e.values[length-1]
		result := 0
		if a != b {
			result = 1
		}
		e.values = append(e.values[:length-2], result)
	case AbsOp:
		length := len(e.values)
		if length < 1 {
			return errors.New("too few arguments for abs")
		}
		a := e.values[length-1]
		e.values = append(e.values[:length-1], int(math.Abs(float64(a))))
	case SqrtOp:
		length := len(e.values)
		if length < 1 {
			return errors.New("too few arguments for sqrt")
		}
		a := e.values[length-1]
		e.values = append(e.values[:length-1], int(math.Sqrt(float64(a))))
	case SinOp:
		length := len(e.values)
		if length < 1 {
			return errors.New("too few arguments for sin")
		}
		a := e.values[length-1]
		e.values = append(e.values[:length-1], int(math.Sin(float64(a))))
	case CosOp:
		length := len(e.values)
		if length < 1 {
			return errors.New("too few arguments for cos")
		}
		a := e.values[length-1]
		e.values = append(e.values[:length-1], int(math.Cos(float64(a))))
	case TanOp:
		length := len(e.values)
		if length < 1 {
			return errors.New("too few arguments for tan")
		}
		a := e.values[length-1]
		e.values = append(e.values[:length-1], int(math.Tan(float64(a))))
	case RandOp:
		e.values = append(e.values, rand.Intn(100))
	default:
		return errors.New("unknown operation")
	}
	e.operations = e.operations[:len(e.operations)-1]
	return nil
}

func (e *Evaluator) GetLastOperation() Operation {
	return e.operations[len(e.operations)-1]
}

func (e *Evaluator) Evaluate(input []Token) error {
	e.values = []int{}

	// Establish a sentinal operation with the lowest precedence (will never be executed)
	e.operations = []Operation{{Sentinal, PrecedenceMap[Sentinal]}}

	for _, token := range input {
		switch token.kind {
		case NumberLiteral:
			err := e.PushValue(token.value)
			if err != nil {
				return err
			}

		case OpenParen:
			e.operations = append(e.operations, Operation{OpenParen, PrecedenceMap[OpenParen]})

		case CloseParen:
			// Evaluate all unevaluated operators until we find a '('
			kind := e.GetLastOperation().kind
			for kind != OpenParen {
				if kind == Sentinal {
					return errors.New("unpaired ')'")
				}
				err := e.Execute()
				if err != nil {
					return err
				}
				kind = e.GetLastOperation().kind
			}
			// Pop the OpenParen
			e.operations = e.operations[:len(e.operations)-1]

			// If this was a function call, make the call
			if e.GetLastOperation().kind > FirstNamedFunction && e.GetLastOperation().kind < LastNamedFunction {
				err := e.Execute()
				if err != nil {
					return err
				}
			}

		case AbsOp, SqrtOp, SinOp, CosOp, TanOp, RandOp:
			// Execut a function call on the arguments
			e.operations = append(e.operations, Operation{token.kind, PrecedenceMap[token.kind]})

		default:
			// By default we're working with a binary operaion
			precedence := PrecedenceMap[token.kind]
			lastOp := e.GetLastOperation()
			for lastOp.precedence >= precedence {
				err := e.Execute()
				if err != nil {
					return err
				}
				lastOp = e.GetLastOperation()
			}
			e.operations = append(e.operations, Operation{token.kind, PrecedenceMap[token.kind]})
		}
	}

	// We hit the end of the expression, evaluate everything
	for {
		switch e.GetLastOperation().kind {
		case OpenParen:
			return errors.New("unpaired '('")
		case Sentinal:
			return nil
		default:
			err := e.Execute()
			if err != nil {
				return err
			}
		}
	}
}

//============================================================================
// This function is the entry point of the "m" library that lexes, parses
// and executes expressions.
// Supports operator precedence and integer math
//============================================================================

func Do(input string) (string, error) {
	// Lex tookens
	lexer := Lexer{}
	lexer.Lex(input)
	e := Evaluator{}
	e.Evaluate(lexer.out)

	if len(e.values) != 1 {
		return "", errors.New("Incorrect operands")
	}

	return fmt.Sprint(e.values[0]), nil
}
