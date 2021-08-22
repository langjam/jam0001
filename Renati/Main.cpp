#include "Common.h"
#include "Lexer.h"
#include "Parser.h"
#include "Interpreter.h"

#include <cstdio>
#include <iostream>

static int RunFile(const char* filepath);
static int Repl();
static void PrintLexResults(std::string_view filePrefix, const std::vector<std::unique_ptr<Token>>& tokens);
static void PrintExpression(const std::string_view filePrefix, const std::unique_ptr<Expression>& expression, size_t level);
static void PrintParseResults(std::string_view filePrefix, const std::vector<std::unique_ptr<Statement>>& statements, size_t level = 0);

int main(int argc, char* argv[])
{
	if (argc == 1)
	{
		return Repl();
	}
	else if (argc == 2)
	{
		return RunFile(argv[1]);
	}
	else
	{
		std::cerr << "Usage: " << argv[0] << " [FILE]\n"
			<< "Omit the file to start REPL\n"
			<< "Expected 0-1 arguments, got " << (argc - 1) << '\n';
		return 1;
	}

	// Remove unused function warnings.
	(void)PrintLexResults;
	(void)PrintExpression;
	(void)PrintParseResults;
}

static int RunFile(const char* const filepath)
{
	std::string code;
	if (!ReadFile(filepath, code))
	{
		std::cerr << "Couldn't read file " << filepath << '\n';
		return 1;
	}

	std::vector<std::unique_ptr<Token>> tokens;
	Error error = Lex(code.c_str(), tokens);
	if (error)
	{
		std::cerr << filepath << ":" << error.pos.line << ":" << error.pos.col << ": Lexer error: " << error.message << '\n';
		return 1;
	}

	// PrintLexResults(filepath, tokens);

	std::vector<std::unique_ptr<Statement>> statements;
	error = Parse(tokens, statements);
	if (error)
	{
		std::cerr << filepath << ":" << error.pos.line << ":" << error.pos.col << ": Parser error: " << error.message << '\n';
		return 1;
	}

	// PrintParseResults(filepath, statements);

	Interpret(filepath, statements);
	return 0;
}

static int Repl()
{
	std::cout << "^C to exit\n";

	std::vector<std::unique_ptr<Token>> tokens;
	std::vector<std::unique_ptr<Statement>> statements;

	bool continuation = false;
	bool eof = false;

	while (true)
	{
		std::cout << (continuation ? ". " : "> ");
		std::string code;
		std::getline(std::cin, code);

		Error error = Lex(code.c_str(), tokens);
		if (error)
		{
			std::cerr << "Lexer error at " << error.pos.line << ':' << error.pos.col << ": " << error.message << '\n';
			continue;
		}

		eof = code == "";

		statements.clear();
		error = Parse(tokens, statements);
		if (error)
		{
			if (eof)
			{
				std::cerr << "Parser error at " << error.pos.line << ':' << error.pos.col << ": " << error.message << '\n';
				tokens.clear();
				continuation = false;
			}
			else
			{
				tokens.pop_back(); // remove EOF token
				continuation = true;
			}
		}
		else
		{
			Interpret("", statements);
			tokens.clear();
			continuation = false;
		}
	}
}

static void PrintLexResults(const std::string_view filePrefix, const std::vector<std::unique_ptr<Token>>& tokens)
{
	for (const auto& token : tokens)
	{
		std::cout << filePrefix << ':' << token->pos.line << ':' << token->pos.col << ':';

		switch (token->tag)
		{
			case TokenTag::KeyVoid: std::cout << "KeyVoid"; break;
			case TokenTag::KeyIf: std::cout << "KeyIf"; break;
			case TokenTag::KeyElif: std::cout << "KeyElif"; break;
			case TokenTag::KeyElse: std::cout << "KeyElse"; break;
			case TokenTag::KeyWhile: std::cout << "KeyWhile"; break;
			case TokenTag::KeyEnd: std::cout << "KeyEnd"; break;
			case TokenTag::KeyFn: std::cout << "KeyFn"; break;
			case TokenTag::KeyReturn: std::cout << "KeyReturn"; break;
			case TokenTag::KeyPush: std::cout << "KeyPush"; break;
			case TokenTag::KeyPop: std::cout << "KeyPop"; break;
			case TokenTag::KeyNot: std::cout << "KeyNot"; break;
			case TokenTag::KeyAnd: std::cout << "KeyAnd"; break;
			case TokenTag::KeyOr: std::cout << "KeyOr"; break;
			case TokenTag::KeyXor: std::cout << "KeyXor"; break;
			case TokenTag::KeyNeg: std::cout << "KeyNeg"; break;
			case TokenTag::KeyFalse: std::cout << "KeyFalse"; break;
			case TokenTag::KeyTrue: std::cout << "KeyTrue"; break;
			case TokenTag::BracketOpen: std::cout << "BracketOpen"; break;
			case TokenTag::BracketClose: std::cout << "BracketClose"; break;
			case TokenTag::ParenOpen: std::cout << "ParenOpen"; break;
			case TokenTag::ParenClose: std::cout << "ParenClose"; break;
			case TokenTag::Plus: std::cout << "Plus"; break;
			case TokenTag::Minus: std::cout << "Minus"; break;
			case TokenTag::Star: std::cout << "Star"; break;
			case TokenTag::Slash: std::cout << "Slash"; break;
			case TokenTag::Percent: std::cout << "Percent"; break;
			case TokenTag::Equals: std::cout << "Equals"; break;
			case TokenTag::LessThan: std::cout << "LessThan"; break;
			case TokenTag::GreaterThan: std::cout << "GreaterThan"; break;
			case TokenTag::LessEquals: std::cout << "LessEquals"; break;
			case TokenTag::GreaterEquals: std::cout << "GreaterEquals"; break;
			case TokenTag::EqualsEquals: std::cout << "EqualsEquals"; break;
			case TokenTag::NotEquals: std::cout << "NotEquals"; break;
			case TokenTag::At: std::cout << "At"; break;
			case TokenTag::Hash: std::cout << "Hash"; break;
			case TokenTag::Number: std::cout << "Number " << static_cast<NumberToken*>(token.get())->value; break;
			case TokenTag::Identifier: std::cout << "Identifier " << static_cast<IdentifierToken*>(token.get())->name; break;
			case TokenTag::Comment:
			{
				std::cout << "Comment\n";

				for (const auto& node : static_cast<CommentToken*>(token.get())->nodes)
				{
					switch (node->tag)
					{
						case CommentNodeTag::Text: std::cout << "\tText " << static_cast<const CommentTextNode&>(*node).text << '\n'; break;
						case CommentNodeTag::Identifier: std::cout << "\tIdentifier " << static_cast<const CommentIdentifierNode&>(*node).name << '\n'; break;
					}
				}
				continue;
			}
			case TokenTag::Eof: std::cout << "EOF"; break;
		}

		std::cout << '\n';
	}
}

static void PrintExpression(const std::string_view filePrefix, const std::unique_ptr<Expression>& expression, const size_t level)
{
	std::cout << filePrefix << ':' << expression->pos.line << ':' << expression->pos.col << ':';
	for (size_t i = 0; i < level; ++i) std::cout << '\t';

	switch (expression->tag)
	{
	case ExpressionTag::False: std::cout << "false"; break;
	case ExpressionTag::True: std::cout << "true"; break;
	case ExpressionTag::NumberLiteral: std::cout << "Number " << static_cast<NumberLiteral*>(expression.get())->value; break;
	case ExpressionTag::ArrayLiteral:
	{
		auto array = static_cast<ArrayLiteral*>(expression.get());
		std::cout << "Array #" << array->values.size() << '\n';
		for (const auto& value : array->values)
		{
			PrintExpression(filePrefix, value, level + 1);
		}
		return;
	}
	case ExpressionTag::FunctionLiteral:
	{
		auto function = static_cast<FunctionLiteral*>(expression.get());
		std::cout << "Function (";
		const size_t n = function->args->size();
		for (size_t i = 0; i < n; ++i)
		{
			std::cout << (*function->args)[i];
			if (i < n - 1) std::cout << ' ';
		}
		std::cout << ")\n";
		PrintParseResults(filePrefix, *function->statements, level + 1);
		return;
	}
	case ExpressionTag::Identifier:
	{
		auto identifier = static_cast<Identifier*>(expression.get());
		std::cout << "Identifier " << identifier->name;
		break;
	}
	case ExpressionTag::Unary:
	{
		auto unary = static_cast<UnaryOperation*>(expression.get());
		std::cout << "Unary " << static_cast<int>(unary->op) << '\n';
		PrintExpression(filePrefix, unary->a, level + 1);
		return;
	}
	case ExpressionTag::Binary:
	{
		auto binary = static_cast<BinaryOperation*>(expression.get());
		std::cout << "Binary " << static_cast<int>(binary->op) << '\n';
		PrintExpression(filePrefix, binary->a, level + 1);
		PrintExpression(filePrefix, binary->b, level + 1);
		return;
	}
	case ExpressionTag::Call:
	{
		auto call = static_cast<Call*>(expression.get());
		std::cout << "Call\n";
		PrintExpression(filePrefix, call->function, level + 1);
		for (const auto& arg : call->values)
		{
			PrintExpression(filePrefix, arg, level + 1);
		}
		return;
	}
	}

	std::cout << '\n';
}

static void PrintParseResults(const std::string_view filePrefix, const std::vector<std::unique_ptr<Statement>>& statements, const size_t level)
{
	for (const auto& statement : statements)
	{
		std::cout << filePrefix << ':' << statement->pos.line << ':' << statement->pos.col << ':';
		for (size_t i = 0; i < level; ++i) std::cout << '\t';

		switch (statement->tag)
		{
		case StatementTag::If:
		{
			auto ifStatement = static_cast<IfStatement*>(statement.get());
			std::cout << "If\n";
			for (const auto& elif : ifStatement->elifChain)
			{
				PrintExpression(filePrefix, elif.condition, level + 1);
				PrintParseResults(filePrefix, elif.statements, level + 1);
			}
			PrintParseResults(filePrefix, ifStatement->elseBlock, level + 1);
			continue;
		}
		case StatementTag::While:
		{
			auto whileStatement = static_cast<WhileStatement*>(statement.get());
			std::cout << "While\n";
			PrintExpression(filePrefix, whileStatement->condition, level + 1);
			PrintParseResults(filePrefix, whileStatement->statements, level + 1);
			continue;
		}
		case StatementTag::Assignment:
		{
			auto assignment = static_cast<AssignmentStatement*>(statement.get());
			std::cout << "Assignment " << assignment->name << '\n';
			PrintExpression(filePrefix, assignment->value, level + 1);
			continue;
		}
		case StatementTag::ArrayWrite:
		{
			auto arrayWrite = static_cast<ArrayWriteStatement*>(statement.get());
			std::cout << "ArrayWrite " << arrayWrite->name << '\n';
			PrintExpression(filePrefix, arrayWrite->index, level + 1);
			PrintExpression(filePrefix, arrayWrite->value, level + 1);
			continue;
		}
		case StatementTag::ArrayPush:
		{
			auto arrayPush = static_cast<ArrayPushStatement*>(statement.get());
			std::cout << "ArrayPush " << arrayPush->name << '\n';
			PrintExpression(filePrefix, arrayPush->value, level + 1);
			continue;
		}
		case StatementTag::ArrayPop:
		{
			auto arrayPop = static_cast<ArrayPopStatement*>(statement.get());
			std::cout << "ArrayPop " << arrayPop->name;
			break;
		}
		case StatementTag::Return:
		{
			auto returnStatement = static_cast<ExpressionStatement*>(statement.get());
			std::cout << "Return\n";
			PrintExpression(filePrefix, returnStatement->value, level + 1);
			continue;
		}
		case StatementTag::Expression:
		{
			auto expressionStatement = static_cast<ExpressionStatement*>(statement.get());
			std::cout << "Expression\n";
			PrintExpression(filePrefix, expressionStatement->value, level + 1);
			continue;
		}
		}
		std::cout << '\n';
	}
}
