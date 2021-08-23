#pragma once

#include "CodePos.h"
#include "Error.h"

#include <cstddef>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

enum class TokenTag {
	KeyVoid,
	KeyIf,
	KeyElif,
	KeyElse,
	KeyWhile,
	KeyEnd,
	KeyFn,
	KeyReturn,
	KeyPush,
	KeyPop,
	KeyNot,
	KeyAnd,
	KeyOr,
	KeyXor,
	KeyNeg,
	KeyFalse,
	KeyTrue,

	BracketOpen,   // [
	BracketClose,  // ]
	ParenOpen,     // (
	ParenClose,    // )

	Plus,          // +
	Minus,         // -
	Star,          // *
	Slash,         // /
	Percent,       // %

	Equals,        // =
	LessThan,      // <
	GreaterThan,   // >
	LessEquals,    // <=
	GreaterEquals, // >=
	EqualsEquals,  // ==
	NotEquals,     // !=

	At,            // @
	Hash,          // #

	Number,
	Identifier,
	Comment,

	Eof,
};

enum class CommentNodeTag {
	Text,
	Identifier,
};

struct Token {
	TokenTag tag;
	CodePos pos;

	Token(const TokenTag tag, const CodePos pos) : tag{tag}, pos{pos} {}
	virtual ~Token() = default;
};

struct NumberToken : public Token {
	double value;

	NumberToken(const double value, const CodePos pos) : Token{TokenTag::Number, pos}, value{value} {}
};

struct IdentifierToken : public Token {
	std::string name;

	IdentifierToken(std::string name, const CodePos pos) : Token{TokenTag::Identifier, pos}, name{std::move(name)} {}
};

struct CommentNode {
	CommentNodeTag tag;

	CommentNode(const CommentNodeTag tag) : tag{tag} {}
	virtual ~CommentNode() = default;

	virtual std::unique_ptr<CommentNode> make_clone() const = 0;
};

struct CommentToken : public Token {
	std::vector<std::unique_ptr<CommentNode>> nodes;

	CommentToken(std::vector<std::unique_ptr<CommentNode>> nodes, const CodePos pos) : Token{TokenTag::Comment, pos}, nodes{std::move(nodes)} {}

	std::unique_ptr<CommentToken> make_clone() const
	{
		std::vector<std::unique_ptr<CommentNode>> nodes_clone;
		nodes_clone.reserve(nodes.size());
		for (const auto& node : nodes) nodes_clone.push_back(node->make_clone());
		return std::make_unique<CommentToken>(std::move(nodes_clone), pos);
	};
};

struct CommentTextNode : public CommentNode {
	std::string text;

	CommentTextNode(std::string text) : CommentNode{CommentNodeTag::Text}, text{text} {}
	std::unique_ptr<CommentNode> make_clone() const override { return std::make_unique<CommentTextNode>(text); }
};

struct CommentIdentifierNode : public CommentNode {
	std::string name;

	CommentIdentifierNode(std::string name) : CommentNode{CommentNodeTag::Identifier}, name{std::move(name)} {}
	std::unique_ptr<CommentNode> make_clone() const override { return std::make_unique<CommentIdentifierNode>(name); }
};

[[nodiscard]] Error Lex(const char* code, std::vector<std::unique_ptr<Token>>& tokens);
