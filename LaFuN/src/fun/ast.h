#pragma once

#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <cstddef>

#include "ByteRange.h"

namespace fun::ast {

struct Identifier {
	std::string name;
	ByteRange range;
	size_t id = 0;
};

struct StringLiteralExpr;
struct NumberLiteralExpr;
struct IdentifierExpr;
struct BinaryExpr;
struct FuncCallExpr;
struct AssignmentExpr;
struct DeclAssignmentExpr;
struct LookupExpr;
using Expression = std::variant<
	StringLiteralExpr,
	NumberLiteralExpr,
	IdentifierExpr,
	BinaryExpr,
	FuncCallExpr,
	AssignmentExpr,
	DeclAssignmentExpr,
	LookupExpr>;

struct StringLiteralExpr {
	std::string str;
};

struct NumberLiteralExpr {
	double num;
};

struct IdentifierExpr {
	Identifier ident;
};

struct BinaryExpr {
	enum Oper {ADD, SUB, MULT, DIV, EQ, NEQ, GT, GTEQ, LT, LTEQ};

	Oper op;
	std::unique_ptr<Expression> lhs;
	std::unique_ptr<Expression> rhs;
};

struct FuncCallExpr {
	std::unique_ptr<Expression> func;
	std::vector<std::unique_ptr<Expression>> args;
};

struct AssignmentExpr {
	std::unique_ptr<Expression> lhs;
	std::unique_ptr<Expression> rhs;
};

struct DeclAssignmentExpr {
	Identifier ident;
	std::unique_ptr<Expression> rhs;
};

struct LookupExpr {
	std::unique_ptr<Expression> lhs;
	std::string name;
};

struct ClassDecl;
struct FuncDecl;
struct MethodDecl;
using Declaration = std::variant<ClassDecl, FuncDecl, MethodDecl>;

struct CodeBlock;
struct IfStatm;
struct WhileStatm;
struct ReturnStatm;
using Statement = std::variant<
	Expression,
	IfStatm,
	WhileStatm,
	ReturnStatm,
	Declaration>;

struct IfStatm {
	Expression condition;
	std::unique_ptr<CodeBlock> ifBody;
	std::unique_ptr<CodeBlock> elseBody;
};

struct WhileStatm {
	Expression condition;
	std::unique_ptr<CodeBlock> body;
};

struct ReturnStatm {
	Expression expr;
};

struct ClassDecl {
	Identifier ident;
	std::vector<Identifier> args;
	std::unique_ptr<CodeBlock> body;
};

struct FuncDecl {
	Identifier ident;
	std::vector<Identifier> args;
	std::unique_ptr<CodeBlock> body;
};

struct MethodDecl {
	Identifier classIdent;
	Identifier ident;
	std::vector<Identifier> args;
	std::unique_ptr<CodeBlock> body;
};

struct CodeBlock {
	std::vector<Statement> statms;
};

}
