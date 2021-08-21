package shared

const TT_int string = "TT_int"
const TT_plus = "TT_plus"
const TT_minus = "TT_minus"
const TT_mul = "TT_mul"
const TT_div = "TT_div"
const TT_lparen = "TT_lparen"
const TT_rparen = "TT_rparen"

type Token struct {
	Type  string
	Value string
}
