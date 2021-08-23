#include "resolve.h"

#include "fun/IdentResolver.h"
#include "ast.h"

using namespace lafun::ast;

namespace lafun {

static size_t findUpwards(LafunDocument &doc, size_t idx, const std::string &name) {
	for (ssize_t i = idx - 1; i >= 0; --i) {
		LafunBlock &block = doc.blocks[i];
		if (std::holds_alternative<FunBlock>(block)) {
			size_t id = fun::resolveUpwardsInDecl(std::get<FunBlock>(block).decl, name);
			if (id != 0) {
				return id;
			}
		}
	}

	for (size_t i = idx + 1; i < doc.blocks.size(); ++i) {
		LafunBlock &block = doc.blocks[i];
		if (std::holds_alternative<FunBlock>(block)) {
			size_t id = fun::resolveDownwardsInDecl(std::get<FunBlock>(block).decl, name);
			if (id != 0) {
				return id;
			}
		}
	}

	return 0;
}

static size_t findDownwards(LafunDocument &doc, size_t idx, const std::string &name) {
	for (size_t i = idx + 1; i < doc.blocks.size(); ++i) {
		LafunBlock &block = doc.blocks[i];
		if (std::holds_alternative<FunBlock>(block)) {
			size_t id = fun::resolveDownwardsInDecl(std::get<FunBlock>(block).decl, name);
			if (id != 0) {
				return id;
			}
		}
	}

	for (ssize_t i = idx - 1; i >= 0; --i) {
		LafunBlock &block = doc.blocks[i];
		if (std::holds_alternative<FunBlock>(block)) {
			size_t id = fun::resolveUpwardsInDecl(std::get<FunBlock>(block).decl, name);
			if (id != 0) {
				return id;
			}
		}
	}

	return 0;
}

void resolveLafunReferences(LafunDocument &document) {
	for (size_t i = 0; i < document.blocks.size(); ++i) {
		auto &block = document.blocks[i];
		if (std::holds_alternative<lafun::ast::IdentifierUpwardsRef>(block)) {
			auto &ref = std::get<lafun::ast::IdentifierUpwardsRef>(block);
			ref.id = findUpwards(document, i, ref.ident);
		} else if (std::holds_alternative<lafun::ast::IdentifierDownwardsRef>(block)) {
			auto &ref = std::get<lafun::ast::IdentifierDownwardsRef>(block);
			ref.id = findDownwards(document, i, ref.ident);
		}
	}

}

}
