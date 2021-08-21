package parser

import (
	"fmt"

	"github.com/grossamos/jam0001/shared"
)

type Parser struct {
	toks          []shared.Token
	open_paren    int
	needed_blocks int
}

func (p *Parser) make_ast() []shared.Node {
	out := make([]shared.Node, 0, 256)

	for len(p.toks) != 0 {
		tok := p.toks[0]

		switch tok.Type {
		case shared.TTopenBlock:
			p.needed_blocks -= 1
		case shared.TTlparen:
			p.open_paren += 1
			p.toks = p.toks[1:]

			out = append(out,
				shared.Node{
					IsExpression: true,
					Children:     p.make_ast()})

			continue

		case shared.TTrparen, shared.TTcloseBlock:
			p.open_paren -= 1
			p.toks = p.toks[1:]
			return out

		case shared.TTinstruction: // (instruction (arguments))
			p.toks = p.toks[1:]

			if len(p.toks) > 0 &&
				p.toks[0].Type == shared.TTlparen {
				p.open_paren += 1

				p.toks = p.toks[1:]
			}

			out = append(out,
				shared.Node{
					IsExpression: true,
					Children: append(
						[]shared.Node{{Val: tok}},
						p.make_ast()...)})

			continue

		case shared.TTstring, shared.TTref:
			if len(p.toks) > 1 &&
				(p.toks)[1].Type == shared.TTlparen {
				p.open_paren += 1
				p.toks = (p.toks)[1:]

				out = append(out,
					shared.Node{
						IsExpression: true,
						Children: append(
							[]shared.Node{{Val: tok}},
							p.make_ast()...)})

				continue
			} else {
				out = append(out, shared.Node{Val: tok})
			}

		case shared.TTwcomment:
			if len(out) == 0 {
				continue
			}

			index := len(out) - 1

			out[index] = shared.Node{
				IsExpression: true,
				Children: []shared.Node{
					{Val: tok},
					out[index]}}

		case shared.TTwhile:
			p.needed_blocks += 1
			if len(p.toks) <= 1 {
				// TODO: add error here
				break
			}

			p.toks = p.toks[1:]
			out = append(out,
				shared.Node{
					IsExpression: true,
					Children: append(
						append(
							[]shared.Node{{Val: tok}},
							p.make_ast()...),
						p.make_ast()...)})

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

func GenerateAst(toks []shared.Token) []shared.Node {
	parser := Parser{toks, 0, 0}
	ast := parser.make_ast()
	fmt.Println(parser.needed_blocks, parser.open_paren)
	return ast
}
