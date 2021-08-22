package m

import (
	"container/list"
	"errors"
	"fmt"
	"math"
	"strconv"
	"strings"
)

//============================================================================
// Text matching functions
//============================================================================

func MatchAny(x byte, chars string) bool {
	for _, v := range chars {
		if x == byte(v) {
			return true
		}
	}
	return false
}

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
	in         string
	match_size int
	out        []Token
}

//============================================================================

func (l *Lexer) Suffix() string {
	if l.match_size != 0 {
		return l.in[l.match_size:]
	}
	return l.in
}

func (l *Lexer) AccumulateMatch(size int) bool {
	l.match_size += size
	return size != 0
}

func (l *Lexer) Eat() bool {
	if l.match_size == 0 {
		return false
	}
	l.in = l.Suffix()
	l.match_size = 0
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
	if l.match_size == 0 {
		return false
	}
	// fmt.Println("Pushing Token \"" + l.in[:l.match_size] + "\"")
	l.out = append(l.out, Token{kind, l.in[:l.match_size]})
	return l.Eat()
}

const (
	Sentinal = iota
	NumberLiteral
	NegativeCellReference
	PositiveCellReference
	OpenParen
	CloseParen
	FunctionCall
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
	values     []int
	operations []Operation
}

func (e *Evaluator) PushValue(text string) error {
	value, err := strconv.Atoi(text)
	if err != nil {
		return errors.New("failed to parse int '" + text + "'")
	}
	e.values = append(e.values, value)
	// fmt.Println(fmt.Sprint(value))
	return nil
}

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
	default:
		return errors.New("unknown operation")
	}
	e.operations = e.operations[:len(e.operations)-1]
	return nil
}

func (e *Evaluator) GetLastOperation() Operation {
	return e.operations[len(e.operations)-1]
}

func (e *Evaluator) Evaluate(input []Token, negativeCells, positiveCells []string) error {
	e.values = []int{}
	e.operations = []Operation{Operation{Sentinal, PrecedenceMap[Sentinal]}}

	for _, token := range input {
		switch token.kind {
		case NumberLiteral:
			err := e.PushValue(token.value)
			if err != nil {
				return err
			}
		case NegativeCellReference:
			// Parse the index from the token.
			index, err := strconv.Atoi(token.value)
			if err != nil {
				return errors.New("failed to parse int '" + token.value + "'")
			}
			if index > len(negativeCells) {
				return errors.New("index -" + fmt.Sprint(index) + " out of range")
			}
			// fmt.Print("Resolved -" + fmt.Sprint(index) + " to ")
			err = e.PushValue(negativeCells[index])
			if err != nil {
				return err
			}
		case PositiveCellReference:
			// Parse the index from the token.
			index, err := strconv.Atoi(token.value)
			if err != nil {
				return errors.New("failed to parse int '" + token.value + "'")
			}
			if index > len(positiveCells) {
				return errors.New("index " + fmt.Sprint(index) + " out of range")
			}
			// fmt.Print("Resolved -" + fmt.Sprint(index) + " to ")
			err = e.PushValue(positiveCells[index])
			if err != nil {
				return err
			}
		case OpenParen:
			e.operations = append(e.operations, Operation{OpenParen, PrecedenceMap[OpenParen]})
		case CloseParen:
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
		default:
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

func Do(input string, negativeCells, positiveCells []string) (string, error) {
	// Lex tookens
	toks := lex(input)
	fmt.Println(toks)

	lexer := Lexer{}
	lexer.Lex(input)
	// fmt.Println("NumTokens = " + fmt.Sprint(len(lexer.out)))
	e := Evaluator{}
	e.Evaluate(lexer.out, negativeCells, positiveCells)
	// fmt.Println("Result = " + fmt.Sprint(e.values[0]))
	return fmt.Sprint(e.values[0]), nil

	// // Replace references
	// toks, err := replaceRefs(toks, negativeCells, positiveCells)
	// if err != nil {
	// 	return "", err
	// }
	// fmt.Println(toks)

	// // Convert to RPN
	// rpn := shuntingYard(toks)
	// fmt.Println(rpn)

	// // Evaluate
	// res, err := evaluate(rpn)
	// if err != nil {
	// 	return "", err
	// }

	// return res, nil
}

func matchIdentifier(x byte) bool {
	return x == '$' || x == '_' || x >= '0' && x <= '9' || x >= 'A' && x <= 'Z' || x >= 'a' && x <= 'z'
}

func lex(input string) []string {
	toks := []string{}

	for len(input) != 0 {
		switch input[0] {
		case ' ':
			input = input[1:]
		case '(', ')', '+', '-', '/', '*', '^':
			toks = append(toks, input[:1])
			input = input[1:]
		case '<', '>', '!', '=':
			if len(input) > 1 && input[1] == '=' {
				toks = append(toks, input[:2])
				input = input[2:]
			} else {
				toks = append(toks, input[:1])
				input = input[1:]
			}
		default:
			length := 0
			for ; length < len(input) && matchIdentifier(input[length]); length++ {
			}
			toks = append(toks, input[:length])
			input = input[length:]
		}
	}

	return toks
}

func replaceRefs(input []string, nCells, pCells []string) ([]string, error) {
	for i := range input {
		if input[i][0] != '$' {
			continue
		}

		fmt.Println("we are here")
		val, err := strconv.Atoi(input[i][1:])
		if err != nil {
			return input, errors.New("incorrect ref in m string")
		}

		arr := pCells
		if val < 0 {
			fmt.Println("using ncells" + fmt.Sprint(val))
			arr = nCells
			val *= -1
		}

		if val > len(arr) {
			return input, errors.New("incorrect ref in m string")
		}

		input[i] = arr[val-1]
	}

	return input, nil
}

const (
	nullT = iota
	numT
	opT
	funT
	popenT
	pcloseT
)

func getType(inp string) int {
	if _, err := strconv.Atoi(inp); err == nil {
		return numT
	}

	if strings.Contains("+ - / * ^ < > ! =", inp) {
		return opT
	}

	if strings.Contains("cos sin tan abs", inp) {
		return funT
	}

	if inp == "(" {
		return popenT
	}

	if inp == ")" {
		return pcloseT
	}

	return nullT
}

var precedenceMap map[string]int = map[string]int{
	"==": 1,
	"!=": 1,
	"<=": 1,
	">=": 1,
	"<":  1,
	">":  1,
	"+":  2,
	"-":  2,
	"*":  3,
	"/":  3,
	"^":  4,
}

func shouldPop(o1, o2 string) bool {
	if o2 == "(" {
		return false
	}

	if o1 == "^" && o2 == "^" {
		return false
	}

	return precedenceMap[o1] <= precedenceMap[o2]
}

func shuntingYard(inp []string) []string {
	opStack := list.New()
	out := []string{}

	for _, tok := range inp {
		switch getType(tok) {
		case numT:
			out = append(out, tok)
		case opT:
			for opStack.Len() > 0 &&
				shouldPop(tok, opStack.Front().Value.(string)) {
				out = append(out, opStack.Front().Value.(string))
				opStack.Remove(opStack.Front())
			}
			opStack.PushFront(tok)
		case popenT, funT:
			opStack.PushFront(tok)
		case pcloseT:
			for opStack.Len() > 0 &&
				opStack.Front().Value.(string) != "(" {
				out = append(out, opStack.Front().Value.(string))
				opStack.Remove(opStack.Front())
			}

			if opStack.Len() > 0 {
				opStack.Remove(opStack.Front())
			}
		}
	}

	for opStack.Len() > 0 {
		out = append(out, opStack.Front().Value.(string))
		opStack.Remove(opStack.Front())
	}

	return out
}

func evaluate(input []string) (string, error) {
	numbers := list.New()

	for i := range input {
		if val, ok := strconv.Atoi(input[i]); ok == nil {
			numbers.PushFront(val)
		} else if numbers.Len() >= 2 {
			a := numbers.Remove(numbers.Front()).(int)
			b := numbers.Remove(numbers.Front()).(int)

			switch input[i] {
			case "+":
				b += a
			case "-":
				b -= a
			case "*":
				b *= a
			case "/":
				if a == 0 {
					return "", errors.New("division by zero")
				}
				b /= a
			case "^":
				b = int(math.Pow(float64(b), float64(a)))
			}

			numbers.PushFront(b)
		} else {
			return "", errors.New("incorrect m statement")
		}
	}

	if numbers.Len() != 1 {
		return "", errors.New("incorrect m statement")
	}

	return strconv.Itoa(numbers.Front().Value.(int)), nil
}
