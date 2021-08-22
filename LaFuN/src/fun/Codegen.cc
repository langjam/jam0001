#include "Codegen.h"

namespace fun {

void Codegen::generate(std::ostream &os) {
	for (const auto &[_, classAndMethods] : classes_) {
		generateClass(os, classAndMethods);
	}

	for (const auto &fun : funs_) {
		generateFun(os, fun);
	}

	for (const auto &statm : statms_) {
		generateStatement(os, statm);
	}
}

void Codegen::generateStatement(std::ostream &os, const ast::Statement *statm) {
	std::visit(overloaded {
		[&](const ast::Declaration &) {}, // decls are handled in a separate code path
		[&](const auto &statm) { generateStatement(os, &statm); }
	}, *statm);
}

void Codegen::generateStatement(std::ostream &os, const ast::Expression *statm) {
	generateExpressionName(os, generateExpression(os, statm));
	os << ";\n";
}

void Codegen::generateStatement(std::ostream &os, const ast::IfStatm *statm) {
	// Temporarily swap out the list of names declared in this scope
	std::unordered_set<std::string> outerDeclaredNames(std::move(alreadyDeclared_));

	os << "{\n";
	auto name = generateExpression(os, &statm->condition);
	os << "if (";
	generateExpressionName(os, name);
	os << ") {\n";
	generateCodeBlock(os, statm->ifBody.get());
	os << "}\n";
	if (statm->elseBody) {
		os << "else {\n";
		generateCodeBlock(os, statm->elseBody.get());
		os << "}\n";
	}
	os << "}\n";

	// Move them back
	alreadyDeclared_ = std::move(outerDeclaredNames);
}

void Codegen::generateStatement(std::ostream &os, const ast::WhileStatm *statm) {
	// Temporarily swap out the list of names declared in this scope
	std::unordered_set<std::string> outerDeclaredNames(std::move(alreadyDeclared_));

	os << "while (true) {\n";
	auto name = generateExpression(os, &statm->condition);
	os << "if (!(";
	generateExpressionName(os, name);
	os << ")) { break; }\n";
	os << "{\n";
	generateCodeBlock(os, statm->body.get());
	os << "}\n";
	os << "}\n";

	// Move them back
	alreadyDeclared_ = std::move(outerDeclaredNames);
}

void Codegen::generateStatement(std::ostream &os, const ast::ReturnStatm *statm) {
	auto name = generateExpression(os, &statm->expr);
	os << "return ";
	generateExpressionName(os, name);
	os << ";\n";
}

void Codegen::generateExpressionName(std::ostream &os, ExpressionName name) {
	std::visit(overloaded {
		[&](TemporaryId temp) { os << "temp" << temp; },
		[&](NameLookup &lookup) { os << "temp" << lookup.first << "." << *lookup.second; },
		[&](const ast::Identifier *temp) { os << "FUN_" << temp->name; },
		[&](const ast::Expression *temp) {
			std::visit(overloaded {
				[&](const ast::StringLiteralExpr &str) { generateStringLiteral(os, str.str); },
				[&](const ast::NumberLiteralExpr &num) { os << num.num; },
				[&](const ast::IdentifierExpr &ident) { os << "FUN_" << ident.ident.name; },
				[&](const auto &) { error("Encountered illegal expression name in codegen"); },
			}, *temp);
		},
	}, name);
}

Codegen::ExpressionName Codegen::generateExpression(std::ostream &os, const ast::Expression *expr) {
	return std::visit(overloaded {
		[&](const ast::StringLiteralExpr &) -> ExpressionName { return expr; },
		[&](const ast::NumberLiteralExpr &) -> ExpressionName { return expr; },
		[&](const ast::IdentifierExpr &) -> ExpressionName { return expr; },
		[&](const ast::BinaryExpr &expr2) -> ExpressionName {
			// Recursively generate the two operands to get the expression names lhs and rhs
			// Emit temp = lhs + rhs
			// Return temp as the expression name
			auto lhsName = generateExpression(os, expr2.lhs.get());
			auto rhsName = generateExpression(os, expr2.rhs.get());
			auto temp = count();
			os << "const temp" << temp << " = ";
			generateExpressionName(os, lhsName);
			switch (expr2.op) {
				case ast::BinaryExpr::EQ: os << " == "; break;
				case ast::BinaryExpr::NEQ: os << " != "; break;
				case ast::BinaryExpr::GT: os << " > "; break;
				case ast::BinaryExpr::GTEQ: os << " >= "; break;
				case ast::BinaryExpr::LT: os << " < "; break;
				case ast::BinaryExpr::LTEQ: os << " <= "; break;
				case ast::BinaryExpr::ADD: os << " + "; break;
				case ast::BinaryExpr::SUB: os << " - "; break;
				case ast::BinaryExpr::MULT: os << " * "; break;
				case ast::BinaryExpr::DIV: os << " / "; break;
			}
			generateExpressionName(os, rhsName);
			os << ";\n";
			return temp;
		},
		[&](const ast::FuncCallExpr &expr2) -> ExpressionName {
			// Recursively generate the function and arguments
			// Emit temp = fun(args...)
			// Return temp as the expression name
			auto funName = generateExpression(os, expr2.func.get());
			std::vector<ExpressionName> argNames;
			for (const auto &arg : expr2.args) {
				argNames.emplace_back(generateExpression(os, arg.get()));
			}
			auto temp = count();
			os << "const temp" << temp << " = ";
			generateExpressionName(os, funName);
			os << "(";
			bool first = true;
			for (const auto &argName : argNames) {
				if (!first) {
					os << ", ";
				}
				generateExpressionName(os, argName);
				first = false;
			}
			os << ");\n";
			return temp;
		},
		[&](const ast::AssignmentExpr &expr2) -> ExpressionName {
			// Recursively generate the rhs
			// Emit lhs = rhsName
			// Return lhs as the expression name
			auto rhsName = generateExpression(os, expr2.rhs.get());
			auto lhsName = generateLvalue(os, expr2.lhs.get());
			generateExpressionName(os, lhsName);
			os << " = ";
			generateExpressionName(os, rhsName);
			os << ";\n";
			return lhsName;
		},
		[&](const ast::DeclAssignmentExpr &expr2) -> ExpressionName {
			// Recursively generate the rhs
			// Emit declaration if not already declared
			// Emit lhs = rhsName
			// Return lhs as the expression name
			auto rhsName = generateExpression(os, expr2.rhs.get());
			if (alreadyDeclared_.find(expr2.ident.name) == alreadyDeclared_.end()) {
				alreadyDeclared_.insert(expr2.ident.name);
				os << "let ";
				generateExpressionName(os, &expr2.ident);
				os << ";\n";
			}
			generateExpressionName(os, &expr2.ident);
			os << " = ";
			generateExpressionName(os, rhsName);
			os << ";\n";
			return &expr2.ident;
		},
		[&](const ast::LookupExpr &expr2) -> ExpressionName {
			auto lhsName = generateExpression(os, expr2.lhs.get());
			auto temp = count();
			os << "const temp" << temp << " = ";
			generateExpressionName(os, lhsName);
			os << ";\n";
			return NameLookup{temp, &expr2.name};
		},
	}, *expr);
}

Codegen::ExpressionName Codegen::generateLvalue(std::ostream &os, const ast::Expression *expr) {
	return std::visit(overloaded {
		[&](const ast::IdentifierExpr &) -> ExpressionName { return expr; },
		[&](const ast::LookupExpr &lookup) -> ExpressionName {
			auto lhsName = generateExpression(os, lookup.lhs.get());
			auto temp = count();
			os << "const temp" << temp << " = ";
			generateExpressionName(os, lhsName);
			os << ";\n";
			return NameLookup{temp, &lookup.name};
		},
		[&](const auto &) -> ExpressionName { error("Invalid lvalue in codegen"); },
	}, *expr);
}

void Codegen::generateFun(std::ostream &os, const ast::FuncDecl *fun) {
	os << "function FUN_" << fun->ident.name << "(";
	generateParameters(os, fun->args);
	os << ") {\n";
	generateCodeBlock(os, fun->body.get());
	os << "}\n";
}

void Codegen::generateCodeBlock(std::ostream &os, const ast::CodeBlock *block) {
	Codegen codegen;
	for (const auto &statm : block->statms) {
		codegen.add(&statm);
	}
	codegen.generate(os);
}

void Codegen::generateClass(std::ostream &os, const ClassAndMethods &clas) {
	generateClassStart(os, clas.first);
	for (const auto &[_, method] : clas.second) {
		generateClassMethods(os, method);
	}
	generateClassEnd(os, clas.first);
}

void Codegen::generateClassStart(std::ostream &os, const ast::ClassDecl *clas) {
	os << "class FUNclass_" << clas->ident.name << " {\n";
	os << "constructor (";
	generateParameters(os, clas->args);
	os << ") {\n";
	os << "let FUN_self = this;\n";
	generateCodeBlock(os, clas->body.get());
	os << "}\n";
}

void Codegen::generateParameters(std::ostream &os, const std::vector<ast::Identifier> &args) {
	if (!args.empty()) {
		os << "FUN_" << args[0].name;
		for (size_t i = 1; i < args.size(); i++) {
			os << ", FUN_" << args[i].name;
		}
	}
}

void Codegen::generateClassMethods(std::ostream &os, const ast::MethodDecl *method) {
	os << method->ident.name << "(";
	generateParameters(os, method->args);
	os << ") {\n";
	os << "let FUN_self = this;\n";
	generateCodeBlock(os, method->body.get());
	os << "}\n";
}

void Codegen::generateClassEnd(std::ostream &os, const ast::ClassDecl *clas) {
	os << "}\n";

	os << "function FUN_" << clas->ident.name << "(";
	generateParameters(os, clas->args);
	os << ") {\n";
	os << "return new FUNclass_" << clas->ident.name << "(";
	generateParameters(os, clas->args);
	os << ");\n}\n";
};

void Codegen::generateStringLiteral(std::ostream &os, const std::string &str) {
	os << '"';

	auto hexNibble = [](int nibble) {
		if (nibble > 10) {
			return 'A' + (nibble - 10);
		} else {
			return '0' + nibble;
		}
	};

	for (char ch: str) {
		if (ch == '\\' || ch == '"') {
			os << '\\' << ch;
		} else if (ch < 30) {
			unsigned char uch = ch;
			os << "\\x0" << hexNibble(uch & 0x0f);
		} else {
			os << ch;
		}
	}

	os << '"';
}

}
