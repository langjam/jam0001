package evaluator

import (
	"bufio"
	"os"

	"fmt"

	"github.com/grossamos/jam0001/lexer"
)

const shell_prompt = "〈( ^.^)ノ >>> "

func RunShell() {
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print(shell_prompt)
		input, _ := reader.ReadString('\n')
		// input := "while"

		// actually do some cool stuff
		fmt.Println(lexer.RunLexer(input))
	}
}
