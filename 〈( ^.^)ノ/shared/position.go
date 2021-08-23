package shared

type Position struct {
	Line int
	// Index    int
	// Column   int
	// Filename string
	// Filetext string
}

func (p *Position) Advance() {
	p.Line += 1
}
