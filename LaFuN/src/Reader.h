#pragma once

#include <string>

class Reader {
public:
	Reader(std::string_view string): string_(string) {}

	void reset();
	int readCh();
	int peekCh(size_t n) const;

	size_t idx = 0;
	int line = 0;
	int column = 0;

private:
	std::string_view string_;
};
