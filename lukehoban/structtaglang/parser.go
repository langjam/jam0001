package main

import (
	"fmt"
	"strconv"
	"strings"
	"text/scanner"
)

type Token struct {
	Kind     rune
	String   string
	Position scanner.Position
}

type Parser struct {
	Tokens []Token
	Index  int
}

func NewParser(src string, name string) Parser {
	var s scanner.Scanner
	s.Init(strings.NewReader(src))
	s.Filename = name
	parser := Parser{}
	for tok := s.Scan(); tok != scanner.EOF; tok = s.Scan() {
		parser.Tokens = append(parser.Tokens, Token{Kind: tok, String: s.TokenText(), Position: s.Pos()})
	}
	return parser
}

func (p *Parser) Peek(n int) Token {
	if n+p.Index >= len(p.Tokens) {
		return Token{Kind: scanner.EOF}
	}
	return p.Tokens[n+p.Index]
}

func (p *Parser) Skip(n int) bool {
	if n+p.Index > len(p.Tokens) {
		return false
	}
	p.Index += n
	return true
}

func (p *Parser) Take() Token {
	if p.Index >= len(p.Tokens) {
		return Token{Kind: scanner.EOF}
	}
	tok := p.Tokens[p.Index]
	p.Index++
	return tok
}

func (p *Parser) MustTake(t rune) error {
	tok := p.Take()
	if tok.Kind != t {
		return fmt.Errorf("expected %d(%s) got %d(%s)", t, string(t), tok.Kind, string(tok.Kind))
	}
	return nil
}

func (p *Parser) ParseExpression() (Expression, error) {
	expr, err := p.ParseBasicExpression()
	if err != nil {
		return nil, err
	}
	tok := p.Peek(0)
	switch tok.String {
	case ",":
		return p.ParseTupleExpressiion(expr)
	}
	return expr, nil
}

func (p *Parser) ParseTupleExpressiion(first Expression) (Expression, error) {
	ret := []Expression{first}
	for p.Peek(0).Kind == ',' {
		p.Skip(1)
		item, err := p.ParseBasicExpression()
		if err != nil {
			return nil, err
		}
		ret = append(ret, item)
	}
	return &Tuple{Items: ret}, nil
}

func (p *Parser) ParseBasicExpression() (Expression, error) {
	expr, err := p.ParsePathExpression()
	if err != nil {
		return nil, err
	}
	tok := p.Peek(0)
	switch tok.String {
	case "+", "%", "*", "-", "^", "/", "<", ">", "?":
		p.Skip(1)
		right, err := p.ParseBasicExpression()
		if err != nil {
			return nil, err
		}
		expr = &BinaryOperator{Tok: tok, Left: expr, Right: right}
	case "(":
		p.Skip(1)
		var args []Expression
	L:
		for {
			switch p.Peek(0).String {
			case ")":
				break L
			default:
				argExpr, err := p.ParseBasicExpression()
				if err != nil {
					return nil, err
				}
				args = append(args, argExpr)
				if p.Peek(0).String == "," {
					p.Skip(1)
				}
			}
		}
		expr = &Call{Func: expr, Args: args}
	}
	return expr, nil
}

func (p *Parser) ParsePathExpression() (Expression, error) {
	expr, err := p.ParseSimpleExpression()
	if err != nil {
		return nil, err
	}
	tok := p.Peek(0)
	switch tok.String {
	case ".":
		p.Skip(1)
		idTok := p.Take()
		if idTok.Kind != scanner.Ident {
			panic(fmt.Sprintf("nyi - non-identifier after .: %v", idTok))
		}

		expr = &Lookup{Base: expr, Property: idTok}
	case "[":
		panic("nyi - `foo['bar'] syntax")
	}
	return expr, nil
}

func (p *Parser) ParseSimpleExpression() (Expression, error) {
	tok := p.Take()
	switch tok.Kind {
	case scanner.Ident:
		return &Identifier{Tok: tok}, nil
	case scanner.Int:
		i, err := strconv.Atoi(tok.String)
		if err != nil {
			return nil, err
		}
		return &Literal{Value: i}, nil
	case scanner.Float:
		f, err := strconv.ParseFloat(tok.String, 64)
		if err != nil {
			return nil, err
		}
		return &Literal{Value: f}, nil
	case '(':
		expr, err := p.ParseExpression()
		if err != nil {
			return nil, err
		}
		if err := p.MustTake(')'); err != nil {
			return nil, err
		}
		return expr, nil
	default:
		return nil, fmt.Errorf("simple expression: unexpected token %v", tok)
	}
}
