package evaluator

import (
	"bufio"
	"fmt"
	"os"
)

const shell_prompt = "〈( ^.^)ノ >>> "

func RunShell() {
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print(shell_prompt)
		input, _ := reader.ReadString('\n')

		// actually do some cool stuff
		fmt.Println(input)
	}
}
