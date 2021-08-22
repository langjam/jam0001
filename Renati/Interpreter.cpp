#include "Common.h"
#include "Interpreter.h"

#include "Parser.h"

#include <cmath>
#include <iostream>
#include <unordered_map>
#include <utility>

enum class TypeTag {
	Void,     // Value
	Bool,     // BoolValue
	Number,   // NumberValue
	Array,    // ArrayRef
	Function, // FunctionRef
};

struct Scope;

struct Comment {
	std::unique_ptr<CommentToken> token;
	std::shared_ptr<Scope> scope;

	Comment(const CommentToken& token, std::shared_ptr<Scope> scope) : token{token.make_clone()}, scope{std::move(scope)} {}
};

struct Value {
	TypeTag type;
	std::shared_ptr<Comment> attachedComment;

	explicit Value(const TypeTag type, std::shared_ptr<Comment> attachedComment) : type{type}, attachedComment{std::move(attachedComment)} {}
	virtual ~Value() = default;

	virtual std::unique_ptr<Value> make_clone() const { return std::make_unique<Value>(type, attachedComment); }
};

struct BoolValue : public Value {
	bool value;

	explicit BoolValue(const bool value, std::shared_ptr<Comment> attachedComment) : Value{TypeTag::Bool, std::move(attachedComment)}, value{value} {}
	std::unique_ptr<Value> make_clone() const override { return std::make_unique<BoolValue>(value, attachedComment); }
};

struct NumberValue : public Value {
	double value;

	explicit NumberValue(const double value, std::shared_ptr<Comment> attachedComment) : Value{TypeTag::Number, std::move(attachedComment)}, value{value} {}
	std::unique_ptr<Value> make_clone() const override { return std::make_unique<NumberValue>(value, attachedComment); }
};

struct ArrayRef : public Value {
	std::shared_ptr<std::vector<double>> array;

	explicit ArrayRef(std::shared_ptr<std::vector<double>> array, std::shared_ptr<Comment> attachedComment) : Value{TypeTag::Array, std::move(attachedComment)}, array{std::move(array)} {}
	std::unique_ptr<Value> make_clone() const override { return std::make_unique<ArrayRef>(array, attachedComment); }
};

struct Function {
	std::shared_ptr<std::vector<std::string>> args;
	std::shared_ptr<std::vector<std::unique_ptr<Statement>>> statements;
	std::shared_ptr<Scope> closure;

	Function(std::shared_ptr<std::vector<std::string>> args, std::shared_ptr<std::vector<std::unique_ptr<Statement>>> statements, std::shared_ptr<Scope> closure) : args{std::move(args)}, statements{std::move(statements)}, closure{std::move(closure)} {}
};

struct FunctionRef : public Value {
	std::shared_ptr<Function> function;
	// TODO NOTE Should we attach comments to function/array values or references?

	explicit FunctionRef(std::shared_ptr<Function> function, std::shared_ptr<Comment> attachedComment) : Value{TypeTag::Function, std::move(attachedComment)}, function{std::move(function)} {}
	std::unique_ptr<Value> make_clone() const override { return std::make_unique<FunctionRef>(function, attachedComment); }
};

struct Scope {
	std::unordered_map<std::string, std::unique_ptr<Value>> bindings;
	std::shared_ptr<Scope> parent_scope;

	bool TryGetValue(const std::string& name, std::unique_ptr<Value>*& out)
	{
		out = nullptr;

		auto it = bindings.find(name);
		if (it == bindings.end())
		{
			return parent_scope ? parent_scope->TryGetValue(name, out) : false;
		}
		else
		{
			out = &it->second;
			return true;
		}
	}

	void Void(const std::string& name)
	{
		auto it = bindings.find(name);
		if (it != bindings.end())
		{
			bindings.erase(it);
		}
	}

	void SetValue(const std::string& name, std::unique_ptr<Value> value)
	{
		bindings[name] = std::move(value);
	}
};

static std::shared_ptr<Scope> globalScope = std::make_shared<Scope>();
static struct {
	bool unwind;
	std::unique_ptr<Value> returnValue;
} unwindToken;

[[nodiscard]] static Error RunStatement(const Statement& statement, const std::shared_ptr<Scope>& scope);
[[nodiscard]] static Error Evaluate(Expression& expression, const std::shared_ptr<Scope>& scope, std::unique_ptr<Value>& out);
static void PrintValue(const Value& value, bool inComment);

void Interpret(std::string_view filePrefix, std::vector<std::unique_ptr<Statement>>& statements)
{
	for (const auto& statement : statements)
	{
		Error error = RunStatement(*statement, globalScope);
		if (error)
		{
			std::cerr << filePrefix << ':' << error.pos.line << ':' << error.pos.col << ": " << error.message << '\n';
			return;
		}
		if (unwindToken.unwind)
		{
			std::cerr << "Returned from top-level code.";
			return;
		}
	}
}

[[nodiscard]] static Error RunStatement(const Statement& statement, const std::shared_ptr<Scope>& scope)
{
	switch (statement.tag)
	{
	case StatementTag::If:
	{
		const auto& ifStatement = static_cast<const IfStatement&>(statement);

		for (const auto& elif : ifStatement.elifChain)
		{
			std::unique_ptr<Value> condition;
			TRY(Evaluate(*elif.condition, scope, condition));

			bool conditionValue;

			if (condition->type == TypeTag::Bool)
			{
				conditionValue = static_cast<const BoolValue&>(*condition).value;
			}
			else if (condition->type == TypeTag::Number)
			{
				conditionValue = static_cast<const NumberValue&>(*condition).value != 0.0;
			}
			else
			{
				return Error{"Condition is not a boolean and not a number.", elif.condition->pos};
			}

			if (conditionValue)
			{
				for (const auto& statement : elif.statements)
				{
					TRY(RunStatement(*statement, scope));
					if (unwindToken.unwind) return Error::None;
				}
				return Error::None;
			}
		}

		for (const auto& statement : ifStatement.elseBlock)
		{
			TRY(RunStatement(*statement, scope));
			if (unwindToken.unwind) return Error::None;
		}
		return Error::None;
	}
	case StatementTag::While:
	{
		const auto& whileStatement = static_cast<const WhileStatement&>(statement);

		while (true)
		{
			std::unique_ptr<Value> condition;
			TRY(Evaluate(*whileStatement.condition, scope, condition));

			bool conditionValue;

			if (condition->type == TypeTag::Bool)
			{
				conditionValue = static_cast<const BoolValue&>(*condition).value;
			}
			else if (condition->type == TypeTag::Number)
			{
				conditionValue = static_cast<const NumberValue&>(*condition).value != 0.0;
			}
			else
			{
				return Error{"Loop condition is not a boolean and not a number.", whileStatement.condition->pos};
			}

			if (!conditionValue) return Error::None;

			for (const auto& statement : whileStatement.statements)
			{
				TRY(RunStatement(*statement, scope));
				if (unwindToken.unwind) return Error::None;
			}
		}
	}
	case StatementTag::Assignment:
	{
		const auto& assignment = static_cast<const AssignmentStatement&>(statement);

		std::unique_ptr<Value> value;
		TRY(Evaluate(*assignment.value, scope, value));

		if (value->type == TypeTag::Void) scope->Void(assignment.name);
		else
		{
			if (assignment.attachedComment) value->attachedComment = std::make_shared<Comment>(*assignment.attachedComment, scope);
			scope->SetValue(assignment.name, std::move(value));
		}
		return Error::None;
	}
	case StatementTag::ArrayWrite:
	{
		const auto& arrayWrite = static_cast<const ArrayWriteStatement&>(statement);
		std::unique_ptr<Value>* arrayValue;
		if (!scope->TryGetValue(arrayWrite.name, arrayValue))
		{
			return Error{Format("No array named %s.", arrayWrite.name.c_str()), statement.pos};
		}
		else if ((*arrayValue)->type != TypeTag::Array)
		{
			return Error{Format("%s is not an array.", arrayWrite.name.c_str()), statement.pos};
		}
		else
		{
			const auto& array = static_cast<const ArrayRef&>(**arrayValue);

			std::unique_ptr<Value> index;
			TRY(Evaluate(*arrayWrite.index, scope, index));
			if (index->type != TypeTag::Number)
			{
				return Error{"Index to array is not a number.", arrayWrite.index->pos};
			}

			const size_t indexValue = static_cast<size_t>(static_cast<const NumberValue&>(*index).value);

			if (indexValue >= array.array->size())
			{
				return Error{Format("Array index %zu out of bounds (array length is %zu).", indexValue, array.array->size()), arrayWrite.index->pos};
			}

			std::unique_ptr<Value> value;
			TRY(Evaluate(*arrayWrite.value, scope, value));
			if (value->type != TypeTag::Number)
			{
				return Error{"Value written to array is not a number.", arrayWrite.value->pos};
			}

			(*array.array)[indexValue] = static_cast<const NumberValue&>(*value).value;
			return Error::None;
		}
	}
	case StatementTag::ArrayPush:
	{
		const auto& arrayPush = static_cast<const ArrayPushStatement&>(statement);
		std::unique_ptr<Value>* arrayValue;
		if (!scope->TryGetValue(arrayPush.name, arrayValue))
		{
			return Error{Format("No array named %s.", arrayPush.name.c_str()), statement.pos};
		}
		else if ((*arrayValue)->type != TypeTag::Array)
		{
			return Error{Format("%s is not an array.", arrayPush.name.c_str()), statement.pos};
		}
		else
		{
			const auto& array = static_cast<const ArrayRef&>(**arrayValue);

			std::unique_ptr<Value> value;
			TRY(Evaluate(*arrayPush.value, scope, value));
			if (value->type != TypeTag::Number)
			{
				return Error{"Value pushed is not a number.", arrayPush.value->pos};
			}

			array.array->push_back(static_cast<const NumberValue&>(*value).value);
			return Error::None;
		}
	}
	case StatementTag::ArrayPop:
	{
		const auto& arrayPop = static_cast<const ArrayPopStatement&>(statement);
		std::unique_ptr<Value>* value;
		if (!scope->TryGetValue(arrayPop.name, value))
		{
			return Error{Format("No array named %s.", arrayPop.name.c_str()), statement.pos};
		}
		else if ((*value)->type != TypeTag::Array)
		{
			return Error{Format("%s is not an array.", arrayPop.name.c_str()), statement.pos};
		}
		else
		{
			const auto& array = static_cast<const ArrayRef&>(**value);
			array.array->pop_back();
			return Error::None;
		}
	}
	case StatementTag::Return:
	{
		const auto& returnStatement = static_cast<const ExpressionStatement&>(statement);
		std::unique_ptr<Value> value;
		unwindToken.unwind = true;
		TRY(Evaluate(*returnStatement.value, scope, unwindToken.returnValue));
		if (returnStatement.attachedComment) unwindToken.returnValue->attachedComment = std::make_shared<Comment>(*returnStatement.attachedComment, scope);
		return Error::None;
	}
	case StatementTag::Expression:
	{
		const auto& expressionStatement = static_cast<const ExpressionStatement&>(statement);
		std::unique_ptr<Value> value;
		TRY(Evaluate(*expressionStatement.value, scope, value));
		PrintValue(*value, false);
		return Error::None;
	}
	}
	return Error{"Internal error: Unrecognized statement.", statement.pos};
}

[[nodiscard]] static Error Evaluate(Expression& expression, const std::shared_ptr<Scope>& scope, std::unique_ptr<Value>& out)
{
	switch (expression.tag)
	{
		case ExpressionTag::False:
		{
			std::shared_ptr<Comment> comment = expression.attachedComment ? std::make_shared<Comment>(*expression.attachedComment, scope) : nullptr;
			out = std::make_unique<BoolValue>(false, std::move(comment));
			return Error::None;
		}
		case ExpressionTag::True:
		{
			std::shared_ptr<Comment> comment = expression.attachedComment ? std::make_shared<Comment>(*expression.attachedComment, scope) : nullptr;
			out = std::make_unique<BoolValue>(true, std::move(comment));
			return Error::None;
		}
		case ExpressionTag::NumberLiteral:
		{
			std::shared_ptr<Comment> comment = expression.attachedComment ? std::make_shared<Comment>(*expression.attachedComment, scope) : nullptr;
			out = std::make_unique<NumberValue>(static_cast<const NumberLiteral&>(expression).value, std::move(comment));
			return Error::None;
		}
		case ExpressionTag::ArrayLiteral:
		{
			std::shared_ptr<Comment> comment = expression.attachedComment ? std::make_shared<Comment>(*expression.attachedComment, scope) : nullptr;
			const auto& arrayLiteral = static_cast<const ArrayLiteral&>(expression);
			std::vector<double> array;
			array.reserve(arrayLiteral.values.size());
			for (const auto& valueExpression : arrayLiteral.values)
			{
				std::unique_ptr<Value> value;
				TRY(Evaluate(*valueExpression, scope, value));

				if (value->type != TypeTag::Number)
				{
					return Error{"Array initializer is not a number.", valueExpression->pos};
				}
				array.push_back(static_cast<const NumberValue&>(*value).value);
			}

			out = std::make_unique<ArrayRef>(std::make_shared<std::vector<double>>(std::move(array)), std::move(comment));
			return Error::None;
		}
		case ExpressionTag::FunctionLiteral:
		{
			std::shared_ptr<Comment> comment = expression.attachedComment ? std::make_shared<Comment>(*expression.attachedComment, scope) : nullptr;
			FunctionLiteral& functionLiteral = static_cast<FunctionLiteral&>(expression);
			out = std::make_unique<FunctionRef>(std::make_shared<Function>(functionLiteral.args, functionLiteral.statements, scope), std::move(comment));
			return Error::None;
		}
		case ExpressionTag::Identifier:
		{
			const Identifier& identifier = static_cast<const Identifier&>(expression);
			std::unique_ptr<Value>* value;
			if (!scope->TryGetValue(identifier.name, value))
			{
				out = std::make_unique<Value>(TypeTag::Void, nullptr);
			}
			else
			{
				out = (*value)->make_clone();
			}
			if (expression.attachedComment)
			{
				out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
			}
			return Error::None;
		}
		case ExpressionTag::Unary:
		{
			const UnaryOperation& unaryOp = static_cast<const UnaryOperation&>(expression);

			TRY(Evaluate(*unaryOp.a, scope, out));

			switch (unaryOp.op)
			{
			case TokenTag::KeyNot:
			{
				if (out->type != TypeTag::Bool)
				{
					return Error("Logical not of non-boolean value.", unaryOp.a->pos);
				}

				auto& boolValue = static_cast<BoolValue&>(*out);
				boolValue.value = !boolValue.value;
				if (expression.attachedComment)
				{
					out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				}
				return Error::None;
			}
			case TokenTag::KeyNeg:
			{
				if (out->type != TypeTag::Number)
				{
					return Error("Negation of non-number value.", unaryOp.a->pos);
				}

				auto& numberValue = static_cast<NumberValue&>(*out);
				numberValue.value = -numberValue.value;
				if (expression.attachedComment)
				{
					out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				}
				return Error::None;
			}
			case TokenTag::KeyVoid:
				// NOTE We don't skip evaluating voiding expression to allow side effects to happen.
				out = std::make_unique<Value>(TypeTag::Void, out->attachedComment);
				if (expression.attachedComment)
				{
					out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				}
				return Error::None;
			case TokenTag::Hash:
			{
				if (out->type != TypeTag::Array)
				{
					return Error("Array length operator used on non-array value.", unaryOp.a->pos);
				}

				const auto& arrayRef = static_cast<const ArrayRef&>(*out);
				out = std::make_unique<NumberValue>(static_cast<double>(arrayRef.array->size()), arrayRef.attachedComment);
				if (expression.attachedComment)
				{
					out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				}
				return Error::None;
			}
			default:
				return Error{"Internal error: Unrecognized unary operation.", unaryOp.pos};
			}
		}
		case ExpressionTag::Binary:
		{
			const BinaryOperation& binaryOp = static_cast<const BinaryOperation&>(expression);

			TRY(Evaluate(*binaryOp.a, scope, out));

			// NOTE Not evaluating second operand right now because of short-circuit guarantees.

			switch (binaryOp.op)
			{
			case TokenTag::Plus:
			{
				if (out->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.a->pos);

				NumberValue& numberValue = static_cast<NumberValue&>(*out);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				numberValue.value += static_cast<const NumberValue&>(*b).value;
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::Minus:
			{
				if (out->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.a->pos);

				NumberValue& numberValue = static_cast<NumberValue&>(*out);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				numberValue.value -= static_cast<const NumberValue&>(*b).value;
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::Star:
			{
				if (out->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.a->pos);

				NumberValue& numberValue = static_cast<NumberValue&>(*out);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				numberValue.value *= static_cast<const NumberValue&>(*b).value;
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::Slash:
			{
				if (out->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.a->pos);

				NumberValue& numberValue = static_cast<NumberValue&>(*out);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				numberValue.value /= static_cast<const NumberValue&>(*b).value;
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::Percent:
			{
				if (out->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.a->pos);

				NumberValue& numberValue = static_cast<NumberValue&>(*out);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				const double bValue = static_cast<const NumberValue&>(*b).value;
				numberValue.value = fmod(fmod(numberValue.value, bValue) + bValue, bValue);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::KeyAnd:
			{
				if (out->type != TypeTag::Bool) return Error("Logical operand is not boolean.", binaryOp.a->pos);

				BoolValue& boolValue = static_cast<BoolValue&>(*out);

				// short-circuit
				if (!boolValue.value) return Error::None;

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Bool) return Error("Logic operand is not boolean.", binaryOp.b->pos);

				boolValue.value = boolValue.value && static_cast<const BoolValue&>(*b).value;
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::KeyOr:
			{
				if (out->type != TypeTag::Bool) return Error("Logic operand is not boolean.", binaryOp.a->pos);

				BoolValue& boolValue = static_cast<BoolValue&>(*out);

				// short-circuit
				if (boolValue.value) return Error::None;

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Bool) return Error("Logic operand is not boolean.", binaryOp.b->pos);

				boolValue.value = boolValue.value || static_cast<const BoolValue&>(*b).value;
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::KeyXor:
			{
				if (out->type != TypeTag::Bool) return Error("Logic operand is not boolean.", binaryOp.a->pos);

				BoolValue& boolValue = static_cast<BoolValue&>(*out);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Bool) return Error("Logic operand is not boolean.", binaryOp.b->pos);

				boolValue.value = boolValue.value != static_cast<const BoolValue&>(*b).value;
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::LessThan:
			{
				if (out->type != TypeTag::Number) return Error("Comparison operand is not a number.", binaryOp.a->pos);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				out = std::make_unique<BoolValue>(static_cast<NumberValue&>(*out).value < static_cast<const NumberValue&>(*b).value, out->attachedComment);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::GreaterThan:
			{
				if (out->type != TypeTag::Number) return Error("Comparison operand is not a number.", binaryOp.a->pos);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				out = std::make_unique<BoolValue>(static_cast<NumberValue&>(*out).value > static_cast<const NumberValue&>(*b).value, out->attachedComment);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::LessEquals:
			{
				if (out->type != TypeTag::Number) return Error("Comparison operand is not a number.", binaryOp.a->pos);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				out = std::make_unique<BoolValue>(static_cast<NumberValue&>(*out).value <= static_cast<const NumberValue&>(*b).value, out->attachedComment);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::GreaterEquals:
			{
				if (out->type != TypeTag::Number) return Error("Comparison operand is not a number.", binaryOp.a->pos);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				out = std::make_unique<BoolValue>(static_cast<NumberValue&>(*out).value >= static_cast<const NumberValue&>(*b).value, out->attachedComment);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::EqualsEquals:
			{
				if (out->type != TypeTag::Number) return Error("Comparison operand is not a number.", binaryOp.a->pos);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				out = std::make_unique<BoolValue>(static_cast<NumberValue&>(*out).value == static_cast<const NumberValue&>(*b).value, out->attachedComment);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::NotEquals:
			{
				if (out->type != TypeTag::Number) return Error("Comparison operand is not a number.", binaryOp.a->pos);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Arithmetic operand is not a number.", binaryOp.b->pos);

				out = std::make_unique<BoolValue>(static_cast<NumberValue&>(*out).value != static_cast<const NumberValue&>(*b).value, out->attachedComment);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			case TokenTag::At:
			{
				if (out->type != TypeTag::Array) return Error("Array read array operand is not an array.", binaryOp.a->pos);

				const ArrayRef& array = static_cast<const ArrayRef&>(*out);

				std::unique_ptr<Value> b;
				TRY(Evaluate(*binaryOp.b, scope, b));
				if (b->type != TypeTag::Number) return Error("Array read index operand is not a number.", binaryOp.b->pos);

				const size_t indexValue = static_cast<size_t>(static_cast<const NumberValue&>(*b).value);

				if (indexValue >= array.array->size())
				{
					return Error{Format("Array index %zu out of bounds (array length is %zu).", indexValue, array.array->size()), binaryOp.b->pos};
				}

				out = std::make_unique<NumberValue>((*array.array)[indexValue], out->attachedComment);
				if (expression.attachedComment) out->attachedComment = std::make_shared<Comment>(*expression.attachedComment, scope);
				else if (out->attachedComment && b->attachedComment) out->attachedComment = nullptr;
				else if (b->attachedComment) out->attachedComment = b->attachedComment;
				return Error::None;
			}
			default:
				return Error{"Internal error: Unrecognized binary operation.", binaryOp.pos};
			}
		}
		case ExpressionTag::Call:
		{
			const Call& call = static_cast<const Call&>(expression);

			std::unique_ptr<Value> functionValue;
			TRY(Evaluate(*call.function, scope, functionValue));
			if (functionValue->type != TypeTag::Function)
			{
				return Error("Call on a a non-function value.", call.function->pos);
			}

			const auto& functionRef = static_cast<const FunctionRef&>(*functionValue);
			const Function& function = *functionRef.function;

			if (function.args->size() != call.values.size())
			{
				return Error(Format("Provided %zu argument(s) for function that takes %zu.", call.values.size(), function.args->size()), call.pos);
			}

			std::shared_ptr<Scope> innerScope = std::make_shared<Scope>();
			const size_t n = call.values.size();
			for (size_t i = 0; i < n; ++i)
			{
				Expression& argExpression = *call.values[i];
				std::unique_ptr<Value> argValue;
				TRY(Evaluate(argExpression, scope, argValue));
				innerScope->SetValue((*function.args)[i], std::move(argValue));
			}
			innerScope->parent_scope = function.closure;

			for (const auto& statement : *function.statements)
			{
				TRY(RunStatement(*statement, innerScope));
				if (unwindToken.unwind)
				{
					unwindToken.unwind = false;
					out = std::move(unwindToken.returnValue);
					return Error::None;
				}
			}
			out = std::make_unique<Value>(TypeTag::Void, nullptr);
			return Error::None;
		}
	}
	return Error{"Internal error: Unrecognized expression.", expression.pos};
}

static void PrintValue(const Value& value, const bool inComment)
{
	if (!inComment && value.attachedComment)
	{
		std::cout << "/*";
		for (const auto& node : value.attachedComment->token->nodes)
		{
			switch(node->tag)
			{
				case CommentNodeTag::Text: std::cout << static_cast<const CommentTextNode&>(*node).text; break;
				case CommentNodeTag::Identifier:
				{
					const auto& identifierNode = static_cast<const CommentIdentifierNode&>(*node);
					std::unique_ptr<Value>* referencedValue;
					if (!value.attachedComment->scope->TryGetValue(identifierNode.name, referencedValue))
					{
						std::cout << "void";
					}
					else
					{
						PrintValue(**referencedValue, true);
					}
				}
			}
		}
		std::cout << "*/\n";
	}

	switch(value.type)
	{
	case TypeTag::Void:
		return;
	case TypeTag::Bool:
	{
		auto const& boolValue = static_cast<const BoolValue&>(value);
		std::cout << (boolValue.value ? "true" : "false");
		break;
	}
	case TypeTag::Number:
	{
		auto const& numberValue = static_cast<const NumberValue&>(value);
		std::cout << numberValue.value;
		break;
	}
	case TypeTag::Array:
	{
		auto const& arrayRef = static_cast<const ArrayRef&>(value);
		const std::vector<double>& array = *arrayRef.array;
		std::cout << '[';
		const size_t n = array.size();
		for (size_t i = 0; i < n; ++i)
		{
			std::cout << array[i];
			if (i < n - 1) std::cout << ' ';
		}
		std::cout << "]";
		break;
	}
	case TypeTag::Function:
	{
		auto const& functionRef = static_cast<const FunctionRef&>(value);
		const Function& function = *functionRef.function;
		std::cout << "fn (";
		const size_t n = function.args->size();
		for (size_t i = 0; i < n; ++i)
		{
			std::cout << (*function.args)[i];
			if (i < n - 1) std::cout << ' ';
		}
		std::cout << ")";
		break;
	}
	}
	if (!inComment) std::cout << '\n';
}
