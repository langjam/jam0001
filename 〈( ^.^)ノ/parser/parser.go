package parser

import (
	"github.com/grossamos/jam0001/shared"
)

// some parser

func GenerateAst(toks *[]shared.Token) []shared.Node {
	out := make([]shared.Node, 0, 256)

	for len(*toks) != 0 {
		tok := (*toks)[0]

		switch tok.Type {
		case shared.TTlparen, shared.TTopenBlock:
			*toks = (*toks)[1:]

			out = append(out,
				shared.Node{
					IsExpression: true,
					Children:     GenerateAst(toks)})

			continue

		case shared.TTrparen, shared.TTcloseBlock:
			return out

		case shared.TTinstruction: // (instruction (arguments))
			*toks = (*toks)[1:]

			out = append(out,
				shared.Node{
					IsExpression: true,
					Children: append(
						[]shared.Node{{Val: tok}},
						GenerateAst(toks)...)})

			continue

		case shared.TTstring:
			if len(*toks) > 0 &&
				(*toks)[1].Type == shared.TTlparen {

				*toks = (*toks)[1:]

				out = append(out,
					shared.Node{
						IsExpression: true,
						Children: append(
							[]shared.Node{{Val: tok}},
							GenerateAst(toks)...)})

				continue
			} else {
				out = append(out, shared.Node{Val: tok})
			}

		default:
			out = append(out, shared.Node{Val: tok})
		}

		*toks = (*toks)[1:]
	}

	return out
}

func RunParser() {

}
