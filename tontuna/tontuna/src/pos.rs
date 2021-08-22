#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct Pos {
    line: u32,
    col: u32,
    src_offset: u32,
}

impl Pos {
    pub const START: Pos = Pos { line: 1, col: 1, src_offset: 0 };

    pub(crate) fn new(line: u32, col: u32, src_offset: u32) -> Pos {
        Pos { line, col, src_offset }
    }

    pub(crate) fn plus_text(mut self, text: &str) -> Pos {
        for ch in text.chars() {
            if ch == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        self.src_offset += text.len() as u32;
        self
    }

    pub(crate) fn plus_char(self, c: char) -> Pos {
        let mut buf = [0; 4];
        self.plus_text(c.encode_utf8(&mut buf))
    }

    pub(crate) fn source_pos(self) -> usize {
        self.src_offset as usize
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub struct Span {
    pub(crate) start: Pos,
    pub(crate) end: Pos,
}

impl Span {
    pub(crate) fn new(start: Pos, end: Pos) -> Span {
        Span { start, end }
    }

    pub(crate) fn start(self) -> Pos {
        self.start
    }

    pub(crate) fn end(self) -> Pos {
        self.end
    }

    pub(crate) fn merge(self, other: Span) -> Span {
        Span {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
    }

    pub fn start_line(self) -> u32 {
        self.start.line
    }

    pub fn source_range(self) -> std::ops::Range<usize> {
        self.start.source_pos()..self.end.source_pos()
    }
}
