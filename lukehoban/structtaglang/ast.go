package main

type ExpressionKind string

const (
	CallExpression           ExpressionKind = "Call"
	LiteralExpression        ExpressionKind = "Literal"
	BinaryOperatorExpression ExpressionKind = "BinaryOperator"
	IdentifierExpression     ExpressionKind = "Identifier"
	TupleExpression          ExpressionKind = "Tuple"
	LookupExpression         ExpressionKind = "Lookup"
)

type Expression interface {
	Kind() ExpressionKind
}

type Call struct {
	Func Expression
	Args []Expression
}

func (*Call) Kind() ExpressionKind { return CallExpression }

type Literal struct {
	Value interface{}
}

func (*Literal) Kind() ExpressionKind { return LiteralExpression }

type BinaryOperator struct {
	Tok   Token
	Left  Expression
	Right Expression
}

func (*BinaryOperator) Kind() ExpressionKind { return BinaryOperatorExpression }

type Identifier struct {
	Tok Token
}

func (*Identifier) Kind() ExpressionKind { return IdentifierExpression }

type Tuple struct {
	Items []Expression
}

func (*Tuple) Kind() ExpressionKind { return TupleExpression }

type Lookup struct {
	Base     Expression
	Property Token
}

func (*Lookup) Kind() ExpressionKind { return LookupExpression }
