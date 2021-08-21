package m

import (
	"container/list"
	"errors"
	"fmt"
	"math"
	"strconv"
	"strings"
)

func Do(input string, negativeCells, positiveCells []string) (string, error) {
	toks := lex(input)
	fmt.Println(toks)
	toks, err := replaceRefs(toks, negativeCells, positiveCells)
	if err != nil {
		return "", err
	}
	fmt.Println(toks)

	rpn := shuntingYard(toks)
	fmt.Println(rpn)
	res, err := evaluate(rpn)
	if err != nil {
		return "", err
	}

	return strconv.Itoa(res), nil
}

func extractTok(input string) int {
	i := 0
	for ; i < len(input) && !strings.Contains("()+-/*^ ", string(input[i])); i++ {
	}
	return i
}

func lex(input string) []string {
	toks := []string{}

	for len(input) != 0 {
		c := input[0]

		switch c {
		case '(', ')', '+', '-', '/', '*', '^':
			toks = append(toks, string(c))
		case ' ':
		default:
			length := extractTok(input)
			toks = append(toks, input[:length])
			input = input[length:]
			continue
		}

		input = input[1:]
	}

	return toks
}

func replaceRefs(input []string, nCells, pCells []string) ([]string, error) {
	for i := range input {
		if input[i][0] != '$' {
			continue
		}

		val, err := strconv.Atoi(input[i][1:])
		if err != nil {
			return input, errors.New("Incorrect ref in m string.")
		}

		arr := pCells
		if val < 0 {
			arr = nCells
			val *= -1
		}

		if val > len(arr) {
			return input, errors.New("Incorrect ref in m string.")
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

	if strings.Contains("+ - / * ^", inp) {
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
	"+": 2,
	"-": 2,
	"*": 3,
	"/": 3,
	"^": 4,
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

func evaluate(input []string) (int, error) {
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
					return 0, errors.New("Division by zero.")
				}
				b /= a
			case "^":
				b = int(math.Pow(float64(b), float64(a)))
			}

			numbers.PushFront(b)
		} else {
			return 0, errors.New("Incorrect m statement.")
		}
	}

	if numbers.Len() != 1 {
		return 0, errors.New("Incorrect m statement.")
	}

	return numbers.Front().Value.(int), nil
}
