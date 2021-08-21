package shared

type Node struct {
	Val         Token
	IsEpression bool
	Children    []Node
}
