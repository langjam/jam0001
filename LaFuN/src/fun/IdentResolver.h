#pragma once

#include <unordered_map>
#include <cstddef>

#include "ast.h"

namespace fun {

struct NameError: public std::exception {
	NameError(std::string message): message(message) {}

	std::string message;

	const char *what() const noexcept override {
		return message.c_str();
	}
};

class IdentResolver;

class ScopeStack {
public:
	ScopeStack(IdentResolver &resolver): resolver_(resolver) {
		scopes_.emplace_back();
	}

	void pushScope();
	void popScope();

	void addDef(ast::Identifier &ident);
	void addRedef(ast::Identifier &ident);
	void addRef(ast::Identifier &ident);

	void addBuiltin(const std::string &name);

	size_t define(const std::string &name);
	size_t redefine(const std::string &name);
	size_t defineTrap(const std::string &name);

	size_t find(const std::string &name);
	size_t tryFind(const std::string &name);

private:
	using Scope = std::unordered_map<std::string, size_t>;
	std::vector<Scope> scopes_;
	IdentResolver &resolver_;

	static constexpr size_t TRAP = ~(size_t)0;
	static constexpr size_t BUILTIN = ~(size_t)1;
};

class IdentResolver {
public:
	void add(ast::Declaration *decl);
	void finalize();
	size_t nextId() { return id_++; }

	void finalizeBlock(ast::CodeBlock &block);

	void addDef(const ast::Identifier *ident) { defs_.push_back(ident); }
	void addRef(const ast::Identifier *ident) { refs_.push_back(ident); }
	void addBuiltin(const std::string &name) { scope_.addBuiltin(name); }

	const std::vector<const ast::Identifier *> &getDefs() const { return defs_; }
	const std::vector<const ast::Identifier *> &getRefs() const { return refs_; }

private:
	std::vector<ast::Declaration *> decls_;
	std::vector<const ast::Identifier *> defs_;
	std::vector<const ast::Identifier *> refs_;
	size_t id_ = 1;
	ScopeStack scope_{*this};
};

size_t resolveUpwardsInDecl(ast::Declaration &decl, const std::string &name);
size_t resolveDownwardsInDecl(ast::Declaration &decl, const std::string &name);

}
