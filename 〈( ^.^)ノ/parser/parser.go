package parser

import (
	"github.com/grossamos/jam0001/shared"
)

type Parser struct {
	toks []shared.Token
}

func (p *Parser) make_ast() []shared.Node {
	out := make([]shared.Node, 0, 256)

	for len(p.toks) != 0 {
		tok := p.toks[0]

		switch tok.Type {
		case shared.TTlparen, shared.TTopenBlock:
			p.toks = p.toks[1:]

			out = append(out,
				shared.Node{
					IsExpression: true,
					Children:     p.make_ast()})

			continue

		case shared.TTrparen, shared.TTcloseBlock:
			p.toks = p.toks[1:]
			return out

		case shared.TTinstruction: // (instruction (arguments))
			p.toks = p.toks[1:]

			if len(p.toks) > 0 &&
				p.toks[0].Type == shared.TTlparen {

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
			if len(p.toks) <= 1 {
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
	parser := Parser{toks}
	return parser.make_ast()
}
