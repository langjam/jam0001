#include "Common.h"
#include "Lexer.h"

#include <cstdint>
#include <cstdlib>
#include <iostream>

using namespace std::literals;

struct CodePtr {
	const char* ptr;
	CodePos pos;

	CodePtr(const char* const ptr) : ptr{ptr}, pos{1, 1} {}

	const char& operator[](const size_t index) const { return ptr[index]; }

	CodePtr& operator+=(const size_t count)
	{
		for (size_t i = 0; i < count; ++i, ++ptr)
		{
			const char c = *ptr;
			if (c == '\n')
			{
				pos.line += 1;
				pos.col = 1;
			}
			else
			{
				pos.col += 1;
			}
		}
		return *this;
	}
};

constexpr size_t KEYWORD_COUNT = 17;
constexpr std::string_view KEYWORDS[KEYWORD_COUNT] = {
	"void"sv,
	"if"sv,
	"elif"sv,
	"else"sv,
	"while"sv,
	"end"sv,
	"fn"sv,
	"return"sv,
	"push"sv,
	"pop"sv,
	"not"sv,
	"and"sv,
	"or"sv,
	"xor"sv,
	"neg"sv,
	"false"sv,
	"true"sv,
};

static bool success;

static void SkipWhitespace(CodePtr& ptr);
static bool IsIdentifierStart(char c);
static bool IsIdentifierMiddle(char c);
static bool IsDigit(char c);
[[nodiscard]] static Error LexComment(CodePtr& ptr, std::unique_ptr<Token>& out);
[[nodiscard]] static Error LexIdentifierOrKeyword(CodePtr& ptr, std::unique_ptr<Token>& out);
[[nodiscard]] static Error LexNumber(CodePtr& ptr, std::unique_ptr<Token>& out);

[[nodiscard]] Error Lex(const char* const code, std::vector<std::unique_ptr<Token>>& tokens)
{
	std::unique_ptr<Token> token;
	CodePtr codePtr{code};

	while (true)
	{
		// whitespace

		SkipWhitespace(codePtr);

		if (codePtr[0] == '\0')
		{
			tokens.emplace_back(std::make_unique<Token>(TokenTag::Eof, codePtr.pos));
			return Error::None;
		}

		// comment

		TRY(LexComment(codePtr, token));
		if (success)
		{
			tokens.emplace_back(std::move(token));
			continue;
		}

		// identifier or keyword

		TRY(LexIdentifierOrKeyword(codePtr, token));
		if (success)
		{
			tokens.emplace_back(std::move(token));
			continue;
		}

		// number

		TRY(LexNumber(codePtr, token));
		if (success)
		{
			tokens.emplace_back(std::move(token));
			continue;
		}

		// simple tokens (2 chars)

		if (codePtr[0] != '\0')
		{
			const uint16_t val = (codePtr[0] << 8) | codePtr[1];
			switch (val)
			{
				case 0x3C3D: tokens.emplace_back(std::make_unique<Token>(TokenTag::LessEquals, codePtr.pos));    codePtr += 2; continue;
				case 0x3E3D: tokens.emplace_back(std::make_unique<Token>(TokenTag::GreaterEquals, codePtr.pos)); codePtr += 2; continue;
				case 0x3D3D: tokens.emplace_back(std::make_unique<Token>(TokenTag::EqualsEquals, codePtr.pos));  codePtr += 2; continue;
				case 0x213D: tokens.emplace_back(std::make_unique<Token>(TokenTag::NotEquals, codePtr.pos));     codePtr += 2; continue;
			}
		}

		// simple tokens (1 char)

		switch (codePtr[0])
		{
			case '[': tokens.emplace_back(std::make_unique<Token>(TokenTag::BracketOpen, codePtr.pos));  codePtr += 1; continue;
			case ']': tokens.emplace_back(std::make_unique<Token>(TokenTag::BracketClose, codePtr.pos)); codePtr += 1; continue;
			case '(': tokens.emplace_back(std::make_unique<Token>(TokenTag::ParenOpen, codePtr.pos));    codePtr += 1; continue;
			case ')': tokens.emplace_back(std::make_unique<Token>(TokenTag::ParenClose, codePtr.pos));   codePtr += 1; continue;
			case '+': tokens.emplace_back(std::make_unique<Token>(TokenTag::Plus, codePtr.pos));         codePtr += 1; continue;
			case '-': tokens.emplace_back(std::make_unique<Token>(TokenTag::Minus, codePtr.pos));        codePtr += 1; continue;
			case '*': tokens.emplace_back(std::make_unique<Token>(TokenTag::Star, codePtr.pos));         codePtr += 1; continue;
			case '/': tokens.emplace_back(std::make_unique<Token>(TokenTag::Slash, codePtr.pos));        codePtr += 1; continue;
			case '%': tokens.emplace_back(std::make_unique<Token>(TokenTag::Percent, codePtr.pos));      codePtr += 1; continue;
			case '=': tokens.emplace_back(std::make_unique<Token>(TokenTag::Equals, codePtr.pos));       codePtr += 1; continue;
			case '<': tokens.emplace_back(std::make_unique<Token>(TokenTag::LessThan, codePtr.pos));     codePtr += 1; continue;
			case '>': tokens.emplace_back(std::make_unique<Token>(TokenTag::GreaterThan, codePtr.pos));  codePtr += 1; continue;
			case '@': tokens.emplace_back(std::make_unique<Token>(TokenTag::At, codePtr.pos));           codePtr += 1; continue;
			case '#': tokens.emplace_back(std::make_unique<Token>(TokenTag::Hash, codePtr.pos));         codePtr += 1; continue;
			default: return Error{"Unrecognized token.", codePtr.pos};
		}
	}
}

static void SkipWhitespace(CodePtr& ptr)
{
	while (true)
	{
		const char c = ptr[0];
		switch (c)
		{
		case '\t':
		case '\r':
		case '\n':
		case ' ':
			ptr += 1;
			break;
		default:
			return;
		}
	}
}

static bool IsIdentifierStart(const char c)
{
	return (c >= 'A' && c <= 'Z')
		|| c == '_'
		|| (c >= 'a' && c <= 'z');
}

static bool IsIdentifierMiddle(const char c)
{
	return (c >= '0' && c <= '9')
		|| (c >= 'A' && c <= 'Z')
		|| c == '_'
		|| (c >= 'a' && c <= 'z');
}

static bool IsDigit(const char c)
{
	return c >= '0' && c <= '9';
}

[[nodiscard]] static Error LexComment(CodePtr& ptr, std::unique_ptr<Token>& out)
{
	if (ptr[0] != '/' || ptr[1] != '*')
	{
		success = false;
		return Error::None;
	}

	const CodePos pos = ptr.pos;

	ptr += 2;

	std::vector<std::unique_ptr<CommentNode>> nodes;
	std::string text;

	while (true)
	{
		switch (ptr[0])
		{
		case '\0':
			return Error{Format("Unexpected EOF, missing */ to close /* (at line %zu, column %zu).", pos.line, pos.col), ptr.pos};
		case '\n':
			text.push_back('\n');
			ptr += 1;
			SkipWhitespace(ptr);
			break;
		case '$':
		{
			ptr += 1;

			if (ptr[0] == '$')
			{
				text.push_back('$');
				ptr += 1;
				break;
			}

			std::unique_ptr<Token> identifier;
			const CodePos identifierPos = ptr.pos;
			TRY(LexIdentifierOrKeyword(ptr, identifier));
			if (!success)
			{
				return Error{"Sequence after \"$\" is not an identifier", identifierPos};
			}
			else if (identifier->tag != TokenTag::Identifier)
			{
				return Error{"Sequence after \"$\" is a reserved keyword, so it's not an identifier", identifierPos};
			}

			if (!text.empty())
			{
				nodes.emplace_back(std::make_unique<CommentTextNode>(std::move(text)));
				text.clear();
			}

			nodes.emplace_back(std::make_unique<CommentIdentifierNode>(std::move(static_cast<const IdentifierToken&>(*identifier).name)));
			break;
		}
		case '*':
		{
			ptr += 1;

			if (ptr[0] != '/')
			{
				text.push_back('*');
				ptr += 1;
				break;
			}
			ptr += 1;

			if (!text.empty())
			{
				nodes.emplace_back(std::make_unique<CommentTextNode>(std::move(text)));
			}

			out = std::make_unique<CommentToken>(std::move(nodes), pos);
			success = true;
			return Error::None;
		}
		default:
			text.push_back(ptr[0]);
			ptr += 1;
			break;
		}
	}
}

[[nodiscard]] static Error LexIdentifierOrKeyword(CodePtr& ptr, std::unique_ptr<Token>& out)
{
	if (!IsIdentifierStart(ptr[0]))
	{
		success = false;
		return Error::None;
	}

	const CodePos pos = ptr.pos;

	const char* const start = &ptr[0];

	size_t length = 0;
	do
	{
		length += 1;
		ptr += 1;
	} while (IsIdentifierMiddle(ptr[0]));

	const std::string_view text{start, length};

	// keyword

	for (size_t i = 0; i < KEYWORD_COUNT; ++i)
	{
		if (text != KEYWORDS[i]) continue;

		success = true;
		out = std::make_unique<Token>(static_cast<TokenTag>(i), pos);
		return Error::None;
	}

	// identifier

	success = true;
	out = std::make_unique<IdentifierToken>(std::string{text}, pos);
	return Error::None;
}

[[nodiscard]] static Error LexNumber(CodePtr& ptr, std::unique_ptr<Token>& out)
{
	if (!IsDigit(ptr[0]))
	{
		success = false;
		return Error::None;
	}

	const CodePos pos = ptr.pos;

	const char* const start = &ptr[0];

	bool hasDot = false;
	ptr += 1;

	while (true)
	{
		const char c = ptr[0];
		if (c == '.')
		{
			if (hasDot) break;

			ptr += 1;
			hasDot = true;
		}

		if (!IsDigit(ptr[0])) break;
		ptr += 1;
	}

	success = true;
	out = std::make_unique<NumberToken>(atof(start), pos);
	return Error::None;
}
