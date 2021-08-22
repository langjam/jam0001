#include "codegen.h"

#include "util.h"
#include "ast.h"

namespace lafun {

static void genFun(std::ostream &os, std::string_view source, size_t start, size_t end);
static void genFunPipe(std::ostream &os);
static void genFunChunk(std::ostream &os, std::string_view source, size_t start, size_t end);
static void genDef(std::ostream &os, const std::string &name, size_t id);
static void genRef(std::ostream &os, const std::string &name, size_t id);

void codegen(std::ostream &os, std::string_view source, const ast::LafunDocument &doc) {
	size_t nextDef = 0;
	size_t nextRef = 0;

	for (const auto &block : doc.blocks) {
		std::visit(overloaded {
			[&](const ast::FunBlock &block2) {
				os << "~\\\\\n{\\parindent0pt\n";
				size_t curByte = block2.range.start;
				while (curByte < block2.range.end) {
					// Determine if the next identifier is a def or a ref
					size_t nextDefStart = -1;
					size_t nextDefEnd = -1;
					size_t nextRefStart = -1;
					size_t nextRefEnd = -1;
					if (nextDef < doc.defs.size()) {
						nextDefStart = doc.defs[nextDef]->range.start;
						nextDefEnd = doc.defs[nextDef]->range.end;
					}
					if (nextRef < doc.refs.size()) {
						nextRefStart = doc.refs[nextRef]->range.start;
						nextRefEnd = doc.refs[nextRef]->range.end;
					}

					if (nextDefStart > block2.range.end && nextRefStart > block2.range.end) {
						// Generate to end of FunBlock
						genFun(os, source, curByte, block2.range.end);
						break;
					}

					// Generate to next identifier, then generate the next identifier
					size_t nextIdentStart;
					size_t nextIdentEnd;
					if (nextDefStart > nextRefStart) {
						nextIdentStart = nextRefStart;
						nextIdentEnd = nextRefEnd;
						genFun(os, source, curByte, nextIdentStart);
						genRef(os, doc.refs[nextRef]->name, doc.refs[nextRef]->id);
						curByte = nextIdentEnd;
						nextRef++;
					} else {
						nextIdentStart = nextDefStart;
						nextIdentEnd = nextDefEnd;
						genFun(os, source, curByte, nextIdentStart);
						genDef(os, doc.defs[nextDef]->name, doc.defs[nextDef]->id);
						curByte = nextIdentEnd;
						nextDef++;
					}
				}
				os << "}\n";
			},
			[&](const ast::RawLatex &block2) { os << block2.str; },
			[&](const ast::IdentifierUpwardsRef &block2) { genRef(os, block2.ident, block2.id); },
			[&](const ast::IdentifierDownwardsRef &block2) { genRef(os, block2.ident, block2.id); },
		}, block);
	}
}

static void genFun(std::ostream &os, std::string_view source, size_t start, size_t end) {
	while (start < end) {
		size_t endOfChunk = start;
		while (endOfChunk < end && source[endOfChunk] != '\n' && source[endOfChunk] != '|') {
			endOfChunk++;
		}
		genFunChunk(os, source, start, endOfChunk);
		start = endOfChunk;
		if (source[start] == '|') {
			genFunPipe(os);
			start++;
		} else if (source[start] == '\n') {
			os << "\\\\\n";
			start++;
		}
	}
}

static void genFunPipe(std::ostream &os) {
	os << "\\lstinline+|+";
}

static void genFunChunk(std::ostream &os, std::string_view source, size_t start, size_t end) {
	os << "\\lstinline|" << source.substr(start, end - start) << "|";
}

static void genDef(std::ostream &os, const std::string &name, size_t id) {
	os << "\\label{lafun-def:" << id << "}\\lstinline|" << name << "|";
}

static void genRef(std::ostream &os, const std::string &name, size_t id) {
	os << "\\hyperref[lafun-def:" << id << "]{\\lstinline|" << name << "|}";
}

}
