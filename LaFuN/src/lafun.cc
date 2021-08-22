#include "lafun/parse.h"
#include "lafun/resolve.h"
#include "lafun/codegen.h"
#include "lafun/print.h"
#include "lafun/prelude.h"
#include "fun/IdentResolver.h"
#include "fun/prelude.h"
#include "fun/Codegen.h"
#include "fun/print.h"
#include "Reader.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <cstring>

bool streq(const char *a, const char *b) {
	return strcmp(a, b) == 0;
}

void usage(const char *argv0) {
	std::cout << "Usage: " << argv0 << " [options] <input file>\n";
	std::cout << "\n";
	std::cout << "Options:\n";
	std::cout << "  --help|-h:          Show this help text\n";
	std::cout << "  --latex <file>:     Write latex to <file>\n";
	std::cout << "  --output|-o <file>: Write generated javascript to <file>\n";
	std::cout << "  --no-latex-prelude: Generate latex code without a prelude\n";
	std::cout << "  --dump-ast:         Dump the parsed syntax tree\n";
}

int main(int argc, const char **argv) {
	std::ofstream jsFile;
	std::ostream *jsStream = nullptr;

	std::ofstream latexFile;
	std::ostream *latexStream = nullptr;

	std::ifstream inputFile;
	std::istream *inputStream = nullptr;

	bool doDumpAst = false;
	bool doAddLatexPrelude = true;

	bool dashes = false;
	for (int i = 1; i < argc; ++i) {
		const char *opt = argv[i];
		if (!dashes && streq(opt, "--")) {
			dashes = true;
		} else if (!dashes && (streq(opt, "-h") || streq(opt, "--help"))) {
			usage(argv[0]);
			return 0;
		} else if (!dashes && streq(opt, "--latex")) {
			if (i == argc - 1) {
				std::cerr << "Option requires an argument: " << opt << '\n';
				return 1;
			}

			if (streq(argv[i + 1], "-")) {
				latexStream = &std::cout;
			} else {
				latexStream = &latexFile;
				latexFile.open(argv[i + 1]);
				if (!latexFile) {
					std::cerr << "Opening file " << argv[i + 1] << " failed\n";
					return 1;
				}
			}

			i += 1;
		} else if (!dashes && (streq(opt, "--output") || streq(opt, "-o"))) {
			if (i == argc - 1) {
				std::cerr << "Option requires an argument: " << opt << '\n';
				return 1;
			}

			if (streq(argv[i + 1], "-")) {
				jsStream = &std::cout;
			} else {
				jsStream = &jsFile;
				jsFile.open(argv[i + 1]);
				if (!jsFile) {
					std::cerr << "Opening file " << argv[i + 1] << " failed\n";
					return 1;
				}
			}

			i += 1;
		} else if (!dashes && streq(opt, "--no-latex-prelude")) {
			doAddLatexPrelude = false;
		} else if (!dashes && streq(opt, "--dump-ast")) {
			doDumpAst = true;
		} else if (!dashes && opt[0] == '-' && opt[1] != '\0') {
			std::cerr << "Unknown option: " << opt << '\n';
			usage(argv[0]);
			return 1;
		} else if (!inputStream) {
			if (streq(opt, "-")) {
				inputStream = &std::cin;
			} else {
				inputStream = &inputFile;
				inputFile.open(opt);
				if (!inputFile) {
					std::cerr << "Opening file " << opt << " failed\n";
					return 1;
				}
			}
		} else {
			std::cerr << "Only one input file, please\n";
			usage(argv[0]);
			return 1;
		}
	}

	if (!inputStream) {
		std::cerr << "Missing required input file option\n";
		usage(argv[0]);
		return 1;
	}

	std::stringstream ss;
	ss << inputFile.rdbuf();
	std::string str = ss.str();

	Reader reader{str};
	fun::IdentResolver resolver;
	for (const std::string &name: fun::preludeNames) {
		resolver.addBuiltin(name);
	}

	lafun::ast::LafunDocument document;
	lafun::parseLafun(reader, document);

	for (auto &block: document.blocks) {
		if (std::holds_alternative<lafun::ast::FunBlock>(block)) {
			resolver.add(&std::get<lafun::ast::FunBlock>(block).decl);
		}
	}

	resolver.finalize();

	document.defs = resolver.getDefs();
	document.refs = resolver.getRefs();

	lafun::resolveLafunReferences(document);

	if (doDumpAst) {
		for (lafun::ast::LafunBlock &block: document.blocks) {
			if (std::holds_alternative<lafun::ast::FunBlock>(block)) {
				lafun::ast::FunBlock &funBlock = std::get<lafun::ast::FunBlock>(block);
				fun::printDeclaration(std::cout, funBlock.decl);
				std::cout << '\n';
			}
		}
	}

	if (jsStream) {
		fun::Codegen gen;
		for (lafun::ast::LafunBlock &block: document.blocks) {
			if (std::holds_alternative<lafun::ast::FunBlock>(block)) {
				lafun::ast::FunBlock &funBlock = std::get<lafun::ast::FunBlock>(block);
				gen.add(&funBlock.decl);
			}
		}

		*jsStream << fun::jsPrelude;
		gen.generate(*jsStream);
		*jsStream << fun::jsPostlude;
	}

	if (latexStream) {
		if (doAddLatexPrelude) {
			*latexStream << lafun::latexPrelude;
		}

		lafun::codegen(*latexStream, str, document);

		if (doAddLatexPrelude) {
			*latexStream << lafun::latexPostlude;
		}
	}

	return 0;
}
