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
		return
	}

	arr := e.positive
	if varIndex < 0 {
		arr = e.negative
		varIndex *= -1
	}

	for len(arr) <= varIndex {
		arr = append(arr, "")
	}

	arr[varIndex-1] = value
}
