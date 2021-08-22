package evaluator

import (
	"fmt"
	"os"
	"strconv"
)

func (e *Evaluator) setRefValue(ref, value string) {
	varIndex, err := strconv.Atoi(ref)
	if err != nil {
		fmt.Println("Incorrect variable reference.")
		os.Exit(1)
	}

	if varIndex == 0 {
		e.zero = value
	}

	arr := e.positive
	if varIndex < 0 {
		arr = e.negative
		varIndex *= -1
	}

	if len(arr) < varIndex {
		fmt.Println("Variable set: out of range.")
		os.Exit(1)
	}

	arr[varIndex] = value
}
