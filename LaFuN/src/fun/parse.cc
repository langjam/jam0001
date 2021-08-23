#include "parse.h"

#include <utility>
#include <cassert>

#include "util.h"

using namespace fun::ast;

namespace fun {

static void fail(Token &tok, TokKind kind) {
	std::string message = "Expected ";
	message += Token::kindToString(kind);
	message += ", got ";
	message += Token::kindToString(tok.kind);
	throw ParseError(tok.line, tok.column, std::move(message));
}

static void fail(Token &tok, std::initializer_list<TokKind> kinds) {
	std::string message = "Expected ";
	bool first = true;
	for (const TokKind &kind: kinds) {
		if (!first) {
			message += " or ";
		}

		message += Token::kindToString(kind);
		first = false;
	}

	message += ", got ";
	message += Token::kindToString(tok.kind);
	throw ParseError(tok.line, tok.column, std::move(message));
}

static void expect(Lexer &lexer, TokKind kind) {
	Token &tok = lexer.peek(0);
	if (tok.kind != kind) {
		fail(tok, kind);
	}
}

static void parseExpression(Lexer &lexer, Expression &expr);

static void parseArgumentList(Lexer &lexer, std::vector<std::unique_ptr<Expression>> &args) {
	lexer.consume(); // '('

	while (true) {
		if (lexer.peek(0).kind == TokKind::CLOSE_PAREN) {
			break;
		}

		std::unique_ptr<Expression> param = std::make_unique<Expression>();
		parseExpression(lexer, *param);
		args.push_back(std::move(param));

		Token &tok = lexer.peek(0);
		if (tok.kind == TokKind::COMMA) {
			lexer.consume(); // ','
		} else if (tok.kind == TokKind::CLOSE_PAREN) {
			break;
		} else {
			fail(tok, {TokKind::COMMA, TokKind::CLOSE_PAREN});
		}
	}

	lexer.consume(); // ')'
}

static void parseExpression(Lexer &lexer, Expression &expr) {
	TokKind kind = lexer.peek(0).kind;
	if (kind == TokKind::STRING) {
		expr = StringLiteralExpr{std::move(lexer.consume().getStr())};
	} else if (kind == TokKind::NUMBER) {
		expr = NumberLiteralExpr{lexer.consume().getNum()};
	} else if (kind == TokKind::IDENT) {
		Token tok = lexer.consume();
		Identifier ident{std::move(tok.getStr()), tok.range};
		expr = IdentifierExpr{ident};
	} else if (kind == TokKind::OPEN_PAREN) {
		Token tok = lexer.consume();
		parseExpression(lexer, expr);
		expect(lexer, TokKind::CLOSE_PAREN);
		lexer.consume();
	} else {
		fail(lexer.peek(0), {
			TokKind::STRING, TokKind::NUMBER, TokKind::IDENT,
			TokKind::OPEN_PAREN
		});
	}

	while (true) {
		kind = lexer.peek(0).kind;
		if (
				kind == TokKind::EQEQ || kind == TokKind::NOTEQ ||
				kind == TokKind::GT || kind == TokKind::GTEQ ||
				kind == TokKind::LT || kind == TokKind::LTEQ ||
				kind == TokKind::PLUS || kind == TokKind::MINUS ||
				kind == TokKind::MULT || kind == TokKind::DIV) {
			lexer.consume(); // operator

			BinaryExpr bin;
			bin.lhs = std::make_unique<Expression>(std::move(expr));
			bin.rhs = std::make_unique<Expression>();
			parseExpression(lexer, *bin.rhs);

			if (kind == TokKind::EQEQ) {
				bin.op = BinaryExpr::EQ;
			} else if (kind == TokKind::NOTEQ) {
				bin.op = BinaryExpr::NEQ;
			} else if (kind == TokKind::GT) {
				bin.op = BinaryExpr::GT;
			} else if (kind == TokKind::GTEQ) {
				bin.op = BinaryExpr::GTEQ;
			} else if (kind == TokKind::LT) {
				bin.op = BinaryExpr::LT;
			} else if (kind == TokKind::LTEQ) {
				bin.op = BinaryExpr::LTEQ;
			} else if (kind == TokKind::PLUS) {
				bin.op = BinaryExpr::ADD;
			} else if (kind == TokKind::MINUS) {
				bin.op = BinaryExpr::SUB;
			} else if (kind == TokKind::MULT) {
				bin.op = BinaryExpr::MULT;
			} else if (kind == TokKind::DIV) {
				bin.op = BinaryExpr::DIV;
			} else {
				assert(false);
			}

			expr = std::move(bin);
		} else if (kind == TokKind::OPEN_PAREN) {
			FuncCallExpr call;
			call.func = std::make_unique<Expression>(std::move(expr));
			parseArgumentList(lexer, call.args);
			expr = std::move(call);
		} else if (kind == TokKind::EQ) {
			lexer.consume(); // '='

			AssignmentExpr assignment;
			assignment.lhs = std::make_unique<Expression>(std::move(expr));
			assignment.rhs = std::make_unique<Expression>();
			parseExpression(lexer, *assignment.rhs);
			expr = std::move(assignment);
		} else if (kind == TokKind::COLONEQ) {
			if (!std::holds_alternative<IdentifierExpr>(expr)) {
				Token &tok = lexer.peek(0);
				throw ParseError(tok.line, tok.column, "':=' must follow an identifier");
			}

			lexer.consume(); // ':='

			Identifier &ident = std::get<IdentifierExpr>(expr).ident;

			DeclAssignmentExpr assignment;
			assignment.ident = ident;
			assignment.rhs = std::make_unique<Expression>();
			parseExpression(lexer, *assignment.rhs);
			expr = std::move(assignment);
		} else if (kind == TokKind::DOT) {
			lexer.consume(); // '.'

			expect(lexer, TokKind::IDENT);
			Token tok = lexer.consume();

			LookupExpr lookup;
			lookup.lhs = std::make_unique<Expression>(std::move(expr));
			lookup.name = std::move(tok.getStr());
			expr = std::move(lookup);
		} else {
			break;
		}
	}
}

static void parseIfStatm(Lexer &lexer, IfStatm &statm) {
	lexer.consume(); // 'if'

	// condition
	parseExpression(lexer, statm.condition);

	// '{'
	expect(lexer, TokKind::OPEN_BRACE);
	lexer.consume(); // '{'

	// <code block>
	statm.ifBody = std::make_unique<CodeBlock>();
	parseCodeBlock(lexer, *statm.ifBody);

	// '}'
	expect(lexer, TokKind::CLOSE_BRACE);
	lexer.consume(); // '}'

	// optional: 'else'
	if (lexer.peek(0).kind == TokKind::ELSE) {
		lexer.consume(); // 'if'
		statm.elseBody = std::make_unique<CodeBlock>();

		// either: '{'
		if (lexer.peek(0).kind == TokKind::OPEN_BRACE) {
			lexer.consume(); // '{'

			// <code block>
			parseCodeBlock(lexer, *statm.elseBody);

			// '}'
			expect(lexer, TokKind::CLOSE_BRACE);
			lexer.consume(); // '}'
		}

		// or: 'if'
		else if (lexer.peek(0).kind == TokKind::IF) {
			// <if statement>
			statm.elseBody->statms.push_back(IfStatm{});
			parseIfStatm(lexer, std::get<IfStatm>(statm.elseBody->statms.back()));

			// Tail-recurse here, don't look for a '}'
			return;
		}

		// or: error
		else {
			fail(lexer.peek(0), {TokKind::OPEN_BRACE, TokKind::IF});
		}
	}
}

static void parseWhileStatm(Lexer &lexer, WhileStatm &statm) {
	lexer.consume(); // 'while'

	// condition
	parseExpression(lexer, statm.condition);

	// '{'
	expect(lexer, TokKind::OPEN_BRACE);
	lexer.consume(); // '{'

	// <code block>
	statm.body = std::make_unique<CodeBlock>();
	parseCodeBlock(lexer, *statm.body);

	// '}'
	expect(lexer, TokKind::CLOSE_BRACE);
	lexer.consume(); // '}'
}

static void parseStatement(Lexer &lexer, Statement &statm) {
	TokKind kind = lexer.peek(0).kind;
	if (kind == TokKind::IF) {
		statm.emplace<IfStatm>();
		parseIfStatm(lexer, std::get<IfStatm>(statm));
	} else if (kind == TokKind::WHILE) {
		statm.emplace<WhileStatm>();
		parseWhileStatm(lexer, std::get<WhileStatm>(statm));
	} else if (kind == TokKind::RETURN) {
		lexer.consume(); // 'return'
		statm.emplace<ReturnStatm>();
		parseExpression(lexer, std::get<ReturnStatm>(statm).expr);
		expect(lexer, TokKind::SEMICOLON);
		lexer.consume(); // ';'
	} else if (kind == TokKind::BACKSLASH) {
		statm.emplace<Declaration>();
		parseDeclaration(lexer, std::get<Declaration>(statm));
	} else {
		statm.emplace<Expression>();
		parseExpression(lexer, std::get<Expression>(statm));
		expect(lexer, TokKind::SEMICOLON);
		lexer.consume(); // ';'
	}
}

void parseCodeBlock(Lexer &lexer, CodeBlock &block) {
	while (true) {
		TokKind kind = lexer.peek(0).kind;
		if (kind == TokKind::E_O_F || kind == TokKind::CLOSE_BRACE) {
			break;
		}

		block.statms.emplace_back();
		parseStatement(lexer, block.statms.back());
	}
}

void parseDeclaration(Lexer &lexer, Declaration &decl) {
	expect(lexer, TokKind::BACKSLASH);
	lexer.consume(); // '\'

	// ident
	expect(lexer, TokKind::IDENT);
	Token identTok = lexer.consume();

	if (identTok.getStr() == "class") {
		expect(lexer, TokKind::OPEN_BRACE);
		lexer.consume(); // '{'

		expect(lexer, TokKind::IDENT);
		Token nameTok = lexer.consume();
		Identifier ident{std::move(nameTok.getStr()), nameTok.range};

		expect(lexer, TokKind::CLOSE_BRACE);
		lexer.consume(); // '}'

		expect(lexer, TokKind::OPEN_BRACE);
		lexer.consume(); // '{'

		// <args>
		std::vector<Identifier> args;
		while (true) {
			if (lexer.peek(0).kind == TokKind::CLOSE_BRACE) {
				break;
			}

			expect(lexer, TokKind::IDENT);
			Token tok = lexer.consume();
			args.push_back({std::move(tok.getStr()), tok.range});

			if (lexer.peek(0).kind == TokKind::COMMA) {
				lexer.consume(); // ','
				continue;
			} else if (lexer.peek(0).kind == TokKind::CLOSE_BRACE) {
				break;
			} else {
				fail(lexer.peek(0), {TokKind::COMMA, TokKind::CLOSE_BRACE});
			}
		}

		lexer.consume(); // '}'

		expect(lexer, TokKind::OPEN_BRACE);
		lexer.consume(); // '{'

		// <code block>
		std::unique_ptr<CodeBlock> body = std::make_unique<CodeBlock>();
		parseCodeBlock(lexer, *body);

		expect(lexer, TokKind::CLOSE_BRACE);
		lexer.consume(); // '}'

		decl = ClassDecl{ident, std::move(args), std::move(body)};
	} else if (identTok.getStr() == "fun") {
		expect(lexer, TokKind::OPEN_BRACE);
		lexer.consume(); // '{'

		expect(lexer, TokKind::IDENT);
		Token name1Tok = lexer.consume();
		Token name2Tok;
		bool isMethod = false;

		if (lexer.peek(0).kind == TokKind::COLONCOLON) {
			isMethod = true;
			lexer.consume(); // '::'

			expect(lexer, TokKind::IDENT);
			name2Tok = lexer.consume();
		}

		expect(lexer, TokKind::CLOSE_BRACE);
		lexer.consume(); // '}'

		expect(lexer, TokKind::OPEN_BRACE);
		lexer.consume(); // '{'

		// <args>
		std::vector<Identifier> args;
		while (true) {
			if (lexer.peek(0).kind == TokKind::CLOSE_BRACE) {
				break;
			}

			expect(lexer, TokKind::IDENT);
			Token tok = lexer.consume();
			args.push_back({std::move(tok.getStr()), tok.range});

			if (lexer.peek(0).kind == TokKind::COMMA) {
				lexer.consume(); // ','
				continue;
			} else if (lexer.peek(0).kind == TokKind::CLOSE_BRACE) {
				break;
			} else {
				fail(lexer.peek(0), {TokKind::COMMA, TokKind::CLOSE_BRACE});
			}
		}

		lexer.consume(); // '}'

		expect(lexer, TokKind::OPEN_BRACE);
		lexer.consume(); // '{'

		// <code block>
		std::unique_ptr<CodeBlock> body = std::make_unique<CodeBlock>();
		parseCodeBlock(lexer, *body);

		expect(lexer, TokKind::CLOSE_BRACE);
		lexer.consume(); // '}'

		if (isMethod) {
			Identifier classIdent{std::move(name1Tok.getStr()), name1Tok.range};
			Identifier ident{std::move(name2Tok.getStr()), name2Tok.range};
			decl = MethodDecl{
				std::move(classIdent), std::move(ident),
				std::move(args), std::move(body),
			};
		} else {
			Identifier ident{std::move(name1Tok.getStr()), name1Tok.range};
			decl = FuncDecl{std::move(ident), std::move(args), std::move(body)};
		}
	} else {
		throw ParseError(
				identTok.line, identTok.column,
				"Expected 'class' or 'fun', got " + identTok.getStr());
	}
}

}
