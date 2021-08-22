#include "parse.h"

#include <algorithm>

#include "fun/parse.h"

using namespace lafun::ast;

namespace lafun {

#define MAX_TOP_LEVEL_KEYWORD_SIZE 5

void parseLafun(Reader &reader, LafunDocument &document) {
	std::string currentBlock;
	while (true) {
		int ch = reader.peekCh(0);
		if (ch == '\\') {
			if (reader.peekCh(1) == '\\') {
				reader.readCh();
				reader.readCh();
				currentBlock += "\\\\";
				continue;
			}

			std::string possibleKeyword;

			size_t i;
			for (i = 1; i < MAX_TOP_LEVEL_KEYWORD_SIZE + 1; i++) {
				int ch = reader.peekCh(i);
				if (!(ch >= 'a' && ch <= 'z')) {
					break;
				}
				possibleKeyword += ch;
			}

			if ((possibleKeyword == "fun" ||
				 possibleKeyword == "class" ||
				 possibleKeyword == "var") &&
				reader.peekCh(i) == '{') {

				if (!currentBlock.empty()) {
					document.blocks.emplace_back(RawLatex{std::move(currentBlock)});
					currentBlock = "";
				}

				size_t startIdx = reader.idx;
				fun::Lexer lexer(reader);
				fun::ast::Declaration decl;
				parseDeclaration(lexer, decl);
				reader = lexer.reader_;
				size_t endIdx = reader.idx;
				document.blocks.emplace_back(FunBlock{std::move(decl), fun::ByteRange{startIdx, endIdx}});
			} else {
				// skip it
				for (size_t j = 0; j < i; j++) {
					currentBlock += reader.readCh();
				}
			}
		} else if (ch == '@' || ch == '!') {
			if (!currentBlock.empty()) {
				document.blocks.emplace_back(RawLatex{std::move(currentBlock)});
			}

			reader.readCh();

			// Parse downwards/upwards ref
			std::string ident;

			while (true) {
				int ch = reader.peekCh(0);
				if (!(
						(ch >= 'a' && ch <= 'z') ||
						(ch >= 'A' && ch <= 'Z') ||
						(ch >= '0' && ch <= '9') ||
						ch == '_')) {
					break;
				}
				ident += reader.readCh();
			}

			if (ch == '@') {
				// Upwards ref
				document.blocks.emplace_back(IdentifierUpwardsRef{std::move(ident)});
			} else {
				// Downwards ref
				document.blocks.emplace_back(IdentifierDownwardsRef{std::move(ident)});
			}
		} else if (ch == '{') {
			// Read till next *matching* }
			currentBlock += reader.readCh();
			size_t numBracesToMatch = 1;
			while (numBracesToMatch > 0) {
				int ch = reader.readCh();
				currentBlock += ch;
				if (ch == EOF) {
					break;
				} else if (ch == '{') {
					numBracesToMatch++;
				} else if (ch == '}') {
					numBracesToMatch--;
				}
			}
		} else if (ch == EOF) {
			if (!currentBlock.empty()) {
				document.blocks.emplace_back(RawLatex{std::move(currentBlock)});
			}
			break;
		} else {
			currentBlock += reader.readCh();
		}
	}
}

}
