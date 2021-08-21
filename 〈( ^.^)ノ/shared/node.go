package shared

type Node struct {
	Val          Token
	IsExpression bool
	Children     []Node
}
