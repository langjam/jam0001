#pragma once

#include <unordered_set>
#include <unordered_map>
#include <string>
#include <variant>
#include <sstream>

#include "util.h"
#include "ast.h"

namespace fun {

struct CodegenError: public std::exception {
	CodegenError(std::string message): error(message) { }

	std::string error;

	const char *what() const noexcept override {
		return error.c_str();
	}
};


class Codegen {
	using Methods = std::unordered_map<std::string, const ast::MethodDecl *>;
	using ClassAndMethods = std::pair<const ast::ClassDecl *, std::unordered_map<std::string, const ast::MethodDecl *>>;
	std::unordered_map<std::string, ClassAndMethods> classes_;
	std::vector<const ast::FuncDecl *> funs_;
	std::vector<const ast::Statement *> statms_; // except decls

	using TemporaryId = size_t;
	using NameLookup = std::pair<TemporaryId, const std::string *>;
	using ExpressionName = std::variant<TemporaryId, NameLookup, const ast::Identifier *, const ast::Expression *>;
	// The name of "x := 5" is the subexpression "x"
	// The name of "foo.bar := 5" is the subexpression "foo.bar" (when we support . operator)
	// The name of "10 + (x := 5)" is some unique size_t not given to any other temporary

public:
	void add(const ast::Statement *statm) {
		if (auto decl = std::get_if<ast::Declaration>(statm)) {
			add(decl);
		} else {
			statms_.push_back(statm);
		}
	}

	void add(const ast::Declaration *decl) {
		std::visit([&](const auto &decl) { add(&decl); }, *decl);
	}

	void add(const ast::ClassDecl *decl) {
		classes_[decl->ident.name].first = decl;
	}

	void add(const ast::MethodDecl *decl) {
		classes_[decl->classIdent.name].second[decl->ident.name] = decl;
	}

	void add(const ast::FuncDecl *decl) {
		funs_.push_back(decl);
	}

	void generate(std::ostream &os);

private:
	std::unordered_set<std::string> alreadyDeclared_;

	void generateStatement(std::ostream &os, const ast::Statement *statm);
	void generateStatement(std::ostream &os, const ast::Expression *statm);
	void generateStatement(std::ostream &os, const ast::IfStatm *statm);
	void generateStatement(std::ostream &os, const ast::WhileStatm *statm);
	void generateStatement(std::ostream &os, const ast::ReturnStatm *statm);

	size_t counter_ = 0;
	size_t count() {
		return counter_++;
	}

	void generateExpressionName(std::ostream &os, ExpressionName name);
	ExpressionName generateExpression(std::ostream &os, const ast::Expression *expr);
	ExpressionName generateLvalue(std::ostream &os, const ast::Expression *expr);
	void generateFun(std::ostream &os, const ast::FuncDecl *fun);
	void generateCodeBlock(std::ostream &os, const ast::CodeBlock *block);
	void generateClass(std::ostream &os, const ClassAndMethods &clas);
	void generateClassStart(std::ostream &os, const ast::ClassDecl *clas);
	void generateParameters(std::ostream &os, const std::vector<ast::Identifier> &args);
	void generateClassMethods(std::ostream &os, const ast::MethodDecl *method);
	void generateClassEnd(std::ostream &os, const ast::ClassDecl *clas);

	void generateStringLiteral(std::ostream &os, const std::string &str);

	[[noreturn]]
	void error(std::string &&message) { throw CodegenError(std::move(message)); }
};

}
