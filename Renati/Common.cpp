#include "Common.h"

#include <cstdio>

bool ReadFile(const char* const filepath, std::string& out)
{
	out.clear();

	FILE* const file = fopen(filepath, "rb");
	if (!file) return false;

	fseek(file, 0, SEEK_END);
	const long size = ftell(file);
	rewind(file);

	out.resize(size);
	fread(out.data(), 1, size, file);
	fclose(file);

	return true;
}

std::string Format(const char* const fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	std::string res = FormatV(fmt, args);

	va_end(args);
	return res;
}

std::string FormatV(const char* const fmt, va_list args)
{
	const int length = vsnprintf(nullptr, 0, fmt, args);

	std::string ret(length, '\0');
	vsnprintf(ret.data(), length, fmt, args);
	return ret;
}
