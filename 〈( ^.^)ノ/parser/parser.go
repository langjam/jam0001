package parser

import (
	"log"
	"strings"

	"github.com/grossamos/jam0001/shared"
)

type Parser struct {
	toks         []shared.Token
	openParen    int
	neededBlocks int
	Comments     map[string]shared.Node
}

func (p *Parser) makeAst() []shared.Node {
	out := make([]shared.Node, 0, 256)

	for len(p.toks) != 0 {
		tok := p.toks[0]

		switch tok.Type {
		case shared.TTlparen, shared.TTopenBlock:
			if tok.Type == shared.TTlparen {
				p.openParen += 1
			} else {
				p.neededBlocks -= 1
			}

			p.toks = p.toks[1:]

			out[len(out)-1].Children = append(
				out[len(out)-1].Children,
				shared.Node{
					IsExpression: true,
					Children:     p.makeAst()})
			continue

		case shared.TTrparen, shared.TTcloseBlock:
			p.openParen -= 1
			p.toks = p.toks[1:]
			return out

		case shared.TTinstruction: // (instruction (arguments))
			p.toks = p.toks[1:]

			if len(p.toks) > 0 &&
				p.toks[0].Type == shared.TTlparen {
				p.openParen += 1

				out = append(out,
					shared.Node{
						IsExpression: true,
						Children:     []shared.Node{{Val: tok}}})
			}
			continue

		case shared.TTstring, shared.TTref:
			if len(p.toks) > 1 &&
				(p.toks)[1].Type == shared.TTlparen {
				p.openParen += 1
				p.toks = (p.toks)[1:]

				out = append(out,
					shared.Node{
						IsExpression: true,
						Children:     []shared.Node{{Val: tok}}})

				continue
			} else {
				out = append(out, shared.Node{Val: tok})
			}

		case shared.TTwcomment:
			if len(out) == 0 {
				continue
			}

			index := len(out) - 1

			if out[index].Children[0].Val.Value == "and" {
				out[index].Children[1] = shared.Node{
					IsExpression: true,
					Children: []shared.Node{
						{Val: tok},
						out[index].Children[1]}}
			} else {
				out[index] = shared.Node{
					IsExpression: true,
					Children: []shared.Node{
						{Val: tok},
						out[index]}}
			}

		case shared.TTwhile:
			p.neededBlocks += 1
			if len(p.toks) <= 1 {
				log.Fatal(MissingBlockError{tok.Pos})
			}

			out = append(out,
				shared.Node{
					IsExpression: true,
					Children:     []shared.Node{{Val: tok}}})

		default:
			out = append(out, shared.Node{Val: tok})
		}

		if len(p.toks) < 1 {
			break
		}

		p.toks = p.toks[1:]
	}

	return out
}

func GenerateAst(toks []shared.Token) ([]shared.Node, map[string]shared.Node) {
	parser := Parser{toks, 0, 0, map[string]shared.Node{}}
	ast := parser.makeAst()
	if parser.neededBlocks != 0 {
		log.Fatal(MissingBlockError{toks[0].Pos})
	}

	parser.Parse(shared.Node{IsExpression: true, Children: ast})

	return ast, parser.Comments
}

func (p *Parser) parseInstruction(ins string, args []shared.Node, pos shared.Position) {
	if !args[0].IsExpression {
		log.Fatal(UnexpectedError{args[0].Val.Value, args[0].Val.Pos})
	}

	if len(args[0].Children) != (map[string]int{
		"set":   2,
		"m":     1,
		"print": 1,
		"not":   1,
		"smile": 0,
		"inz":   4,
		"dnz":   3,
		"and":   2}[ins]) {

		log.Fatal(IncorrectSignatureError{ins, pos})
	}

	if ins == "and" {
		if args[0].Children[0].Val.Type == shared.TTwcomment {
			args[0].Children[0].Val.Type = shared.TTwcommentAnd
			p.Parse(args[0])
		} else {
			log.Fatal(IncorrectSignatureError{ins, pos})
		}

		return
	}

	for _, arg := range args[0].Children {
		p.Parse(arg)
	}
}

func (p *Parser) parseCall(args []shared.Node) {
	if !args[0].IsExpression {
		log.Fatal(UnexpectedError{args[0].Val.Value, args[0].Val.Pos})
	}

	for _, arg := range args[0].Children {
		p.Parse(arg)
	}
}

func (p *Parser) parseComment(content string, add bool, value []shared.Node) {
	p.Parse(value[0])

	content = strings.TrimSpace(content)

	if add {
		p.Comments[content] = shared.Node{
			IsExpression: true,
			Children:     []shared.Node{p.Comments[content], value[0]}}

	} else {
		p.Comments[content] = value[0]
	}
}

func (p *Parser) parseWhile(args []shared.Node) {
	if len(args) != 2 {
		log.Fatal(IncorrectSignatureError{"while", args[0].Val.Pos})
	}

	if !(args[0].IsExpression && args[1].IsExpression) {
		log.Fatal(&IncorrectSignatureError{"while", args[0].Val.Pos})
	}

	p.Parse(args[0])
	p.Parse(args[1])
}

func (p *Parser) Parse(tree shared.Node) {
	if !tree.IsExpression {
		return
	}

	if len(tree.Children) == 0 {
		return
	}

	for i, child := range tree.Children {
		if child.IsExpression {
			p.Parse(child)
			continue
		}

		switch child.Val.Type {
		case shared.TTinstruction:
			p.parseInstruction(child.Val.Value, tree.Children[i+1:], child.Val.Pos)
		case shared.TTstring, shared.TTref:
			p.parseCall(tree.Children[i+1:])
		case shared.TTwcomment, shared.TTwcommentAnd:
			p.parseComment(
				child.Val.Value,
				child.Val.Type == shared.TTwcommentAnd,
				tree.Children[1:])
		case shared.TTwhile:
			p.parseWhile(tree.Children[1:])
		}
		return
	}
}
