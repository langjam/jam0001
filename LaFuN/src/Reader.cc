#include "Reader.h"

void Reader::reset() {
	idx = 0;
	line = 0;
	column = 0;
}

int Reader::readCh() {
	if (idx >= string_.size()) {
		return EOF;
	}

	char ch = string_[idx++];
	column += 1;
	if (ch == '\n') {
		column = 0;
		line += 1;
	}

	return ch;
}

int Reader::peekCh(size_t n) const {
	if (idx + n >= string_.size()) {
		return EOF;
	}

	return string_[idx + n];
}
