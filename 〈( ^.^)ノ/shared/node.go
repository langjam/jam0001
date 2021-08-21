package shared

import (
	"fmt"
)

type Node struct {
	Val          Token
	IsExpression bool
	Children     []Node
}

func (n Node) Print(indent string) {
	if !n.IsExpression {
		fmt.Println(indent, n.Val)
	} else {
		fmt.Println(indent, "(")
		for _, child := range n.Children {
			child.Print(indent + "  ")
		}
		fmt.Println(indent, ")")
	}
}
