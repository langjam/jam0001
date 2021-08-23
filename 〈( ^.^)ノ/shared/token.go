package shared

const (
	TTnull        = iota
	TTlparen      // (
	TTrparen      // )
	TTopenBlock   // {
	TTcloseBlock  // }
	TTinstruction // set, m etc.
	TTstring      // "string"
	TTnumber      // 1, 3.5
	TTconst       // true, false
	TTref         // $number
	TTwhile       // while
	TTwcomment    // comment
	TTwcommentAnd // comment under the and statement
)

type Token struct {
	Type  int
	Value string
	Pos   Position
}
