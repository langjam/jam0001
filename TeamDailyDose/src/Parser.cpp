#include "AK/RefPtr.h"
#include "AST.h"
#include "Parser.h"
#include <AK/Assertions.h>
#include <AK/CharacterTypes.h>
#include <AK/NonnullRefPtr.h>
#include <AK/NonnullRefPtrVector.h>

Result<NonnullRefPtr<StatementListNode>, String> Parser::parse_statement_list(bool toplevel)
{
    NonnullRefPtrVector<ASTNode> statements;

    while (!m_lexer.is_eof()) {
        auto maybe_peek = m_lexer.peek_token();
        if (maybe_peek.is_error())
            return maybe_peek.release_error();
        if (!toplevel && (maybe_peek.value() == TokenType::ParenClose || maybe_peek.value() == TokenType::BracketClose)) {
            // We are after the last statement in a function or block definition.
            break;
        }

        auto maybe_statement = parse_statement(toplevel);
        if (maybe_statement.is_error()) {
            return maybe_statement.release_error();
        }

        statements.append(maybe_statement.release_value());
    }

    return StatementListNode::construct(move(statements));
}

Result<NonnullRefPtr<ASTNode>, String> Parser::parse_statement(bool toplevel)
{
    RefPtr<ASTNode> node;

    auto maybe_peek = m_lexer.peek_token();
    if (maybe_peek.is_error())
        return maybe_peek.release_error();

    switch (maybe_peek.value()) {
    case TokenType::ParenOpen:
    case TokenType::Comment: {
        auto maybe_primary = parse_expression();
        if (maybe_primary.is_error())
            return maybe_primary.release_error();
        node = maybe_primary.release_value();
        break;
    }
    case TokenType::Identifier: {
        if (!toplevel) {
            auto maybe_identifier = m_lexer.next_token();
            if (maybe_identifier.is_error())
                return maybe_identifier.release_error();

            auto maybe_peek = m_lexer.peek_token();
            if (maybe_peek.is_error())
                return maybe_peek.release_error();
            auto has_colon_after_identifier = maybe_peek.value() == TokenType::Colon;

            m_lexer.backtrack(maybe_identifier.value());

            if (has_colon_after_identifier) {
                auto maybe_variable_assignment = parse_variable_assignment();
                if (maybe_variable_assignment.is_error())
                    return maybe_variable_assignment.release_error();

                node = maybe_variable_assignment.release_value();
            } else {
                auto maybe_primary = parse_expression();
                if (maybe_primary.is_error())
                    return maybe_primary.release_error();

                node = maybe_primary.release_value();
            }

            break;
        }

        auto maybe_function_definition = parse_function_definition();
        if (maybe_function_definition.is_error())
            return maybe_function_definition.release_error();
        node = maybe_function_definition.release_value();
        break;
    }
    default:
        return String::formatted("Expected (, \" or identifier at top level, got {}", token_to_string(maybe_peek.value()));
    }

    auto maybe_peek_after_statement = m_lexer.peek_token();
    if (maybe_peek_after_statement.is_error())
        return maybe_peek_after_statement.release_error();

    if (maybe_peek_after_statement.value() != TokenType::Period)
        return String { "Expected statement terminator" };
    m_lexer.consume_token();

    return node.release_nonnull();
}

Result<NonnullRefPtr<ASTNode>, String> Parser::parse_primary()
{
    auto maybe_peek = m_lexer.peek_token();
    if (maybe_peek.is_error())
        return maybe_peek.release_error();

    switch (maybe_peek.value()) {
    case TokenType::Comment: {
        auto maybe_comment = parse_comment();
        if (maybe_comment.is_error())
            return maybe_comment.release_error();
        return static_ptr_cast<ASTNode>(maybe_comment.value());
    }
    case TokenType::Identifier: {
        auto maybe_identifier = parse_identifier();
        if (maybe_identifier.is_error())
            return maybe_identifier.release_error();
        return static_ptr_cast<ASTNode>(maybe_identifier.value());
    }
    case TokenType::BracketOpen: {
        auto maybe_block = parse_block();
        if (maybe_block.is_error())
            return maybe_block.release_error();
        return static_ptr_cast<ASTNode>(maybe_block.value());
    }
    case TokenType::ParenOpen: {
        m_lexer.consume_token();
        auto maybe_expression = parse_expression();
        if (maybe_expression.is_error())
            return maybe_expression.release_error();

        auto maybe_peek_after_expression = m_lexer.peek_token();
        if (maybe_peek_after_expression.is_error())
            return maybe_peek_after_expression.release_error();
        if (maybe_peek_after_expression.value() != TokenType::ParenClose)
            return String::formatted("Expected ) after parenthesized expression, got {}", token_to_string(maybe_peek_after_expression.value()));
        m_lexer.consume_token();

        return static_ptr_cast<ASTNode>(ParenthesizedExpressionNode::construct(maybe_expression.value()));
    }
    default:
        return String::formatted("Expected \" or identifier, got {}", token_to_string(maybe_peek.value()));
    }
}

Result<NonnullRefPtr<ASTNode>, String> Parser::parse_expression()
{
    auto maybe_primary = parse_primary();
    if (maybe_primary.is_error())
        return maybe_primary.release_error();

    NonnullRefPtr<ASTNode> value = maybe_primary.release_value();

    auto maybe_after_comment_peek = m_lexer.peek_token();
    if (maybe_after_comment_peek.is_error())
        return maybe_after_comment_peek.release_error();

    switch (maybe_after_comment_peek.value()) {
    case TokenType::Period:
        // Statement terminator, let's ignore it and let parse_statement
        // handle it.
        break;
    case TokenType::ParenClose:
        // Parenthesized expression.
        break;
    case TokenType::Identifier: {
        auto maybe_message = parse_message_like(value);
        if (maybe_message.is_error())
            return maybe_message.release_error();
        value = maybe_message.release_value();
        break;
    }
    default:
        return String::formatted("Expected ., ) or identifier after expression, got {}", token_to_string(maybe_after_comment_peek.value()));
    }

    return value;
}

Result<NonnullRefPtr<CommentNode>, String> Parser::parse_comment()
{
    auto maybe_comment = m_lexer.next_token();
    if (maybe_comment.is_error())
        return maybe_comment.release_error();

    auto comment = maybe_comment.release_value();
    if (comment.type() != TokenType::Comment) {
        return String::formatted("Expected \", got {}", token_to_string(comment.type()));
    }

    return CommentNode::construct(comment.string());
}

Result<NonnullRefPtr<IdentifierNode>, String> Parser::parse_identifier()
{
    auto maybe_identifier = m_lexer.next_token();
    if (maybe_identifier.is_error())
        return maybe_identifier.release_error();

    auto identifier = maybe_identifier.release_value();
    if (identifier.type() != TokenType::Identifier) {
        return String::formatted("Expected identifier, got {}", token_to_string(identifier.type()));
    }

    return IdentifierNode::construct(identifier.string());
}

Result<NonnullRefPtr<ASTNode>, String> Parser::parse_message_like(NonnullRefPtr<CommentNode> comment)
{
    StringBuilder message_builder;
    NonnullRefPtrVector<ASTNode> arguments;
    NonnullRefPtr<ASTNode> value = comment;

    while (true) {
        auto maybe_peek_for_identifier = m_lexer.peek_token();
        if (maybe_peek_for_identifier.is_error())
            return maybe_peek_for_identifier.release_error();

        if (maybe_peek_for_identifier.value() != TokenType::Identifier)
            break;

        auto maybe_identifier = m_lexer.next_token();
        if (maybe_identifier.is_error())
            return maybe_identifier.release_error();

        auto identifier = maybe_identifier.release_value();

        auto maybe_peek_colon = m_lexer.peek_token();
        if (maybe_peek_colon.is_error())
            maybe_peek_colon.release_error();
        auto has_colon = maybe_peek_colon.value() == TokenType::Colon;

        if (is_ascii_upper_alpha(identifier.string()[0])) {
            if (!has_colon) {
                // Only unary messages get to not have : at the end.
                return String::formatted("Expected : at the end of message argument name, got {}", token_to_string(maybe_peek_colon.value()));
            }

            // First character being uppercase means
            // 1) if we have arguments, a continuation;
            // 2) if we don't have arguments, means this is the upper message's
            //    argument.
            if (arguments.is_empty()) {
                m_lexer.backtrack(identifier);
                break;
            }
        } else {
            if (!arguments.is_empty()) {
                // We should never get here, as a nested parse_expression() call
                // should handle messages of the child expression.
                VERIFY_NOT_REACHED();
            }

            if (!has_colon) {
                // This is our unary message. We have to fold and continue,
                // since more unary messages or a keyword message may follow.
                value = MessageNode::construct(value, identifier.string(), NonnullRefPtrVector<ASTNode> {});
                continue;
            }
        }

        m_lexer.consume_token();
        message_builder.append(identifier.string());
        message_builder.append(':');

        auto maybe_expression = parse_expression();
        if (maybe_expression.is_error())
            return maybe_expression.release_error();
        arguments.append(maybe_expression.value());
    }

    if (!arguments.is_empty()) {
        value = MessageNode::construct(move(value), message_builder.to_string(), move(arguments));
    }

    return value;
}

Result<NonnullRefPtr<FunctionDefinitionNode>, String> Parser::parse_function_definition()
{
    StringBuilder name_builder;
    Vector<String> arguments;

    while (true) {
        auto maybe_peek_identifier = m_lexer.peek_token();
        if (maybe_peek_identifier.is_error())
            return maybe_peek_identifier.release_error();
        if (maybe_peek_identifier.value() != TokenType::Identifier)
            break;

        auto maybe_identifier = m_lexer.next_token();
        if (maybe_identifier.is_error())
            return maybe_identifier.release_error();

        auto identifier = maybe_identifier.release_value();

        auto maybe_peek_colon = m_lexer.peek_token();
        if (maybe_peek_colon.is_error())
            return maybe_peek_colon.release_error();
        auto has_colon = maybe_peek_colon.value() == TokenType::Colon;

        if (!has_colon) {
            if (!name_builder.is_empty()) {
                return String::formatted("Expected : after keyword for non-unary message definition, got {}", token_to_string(maybe_peek_colon.value()));
            }

            auto maybe_peek_paren = m_lexer.peek_token();
            if (maybe_peek_paren.is_error())
                return maybe_peek_paren.release_error();
            if (maybe_peek_paren.value() != TokenType::ParenOpen) {
                return String::formatted("Expected ( after unary message name, got {}", token_to_string(maybe_peek_paren.value()));
            }
        } else {
            if (!name_builder.is_empty() && !is_ascii_upper_alpha(identifier.string()[0])) {
                return String { "First letter of message keywords after the first must be upper-case" };
            }
        }

        name_builder.append(identifier.string());

        if (!has_colon) {
            break;
        }

        m_lexer.consume_token();
        name_builder.append(':');

        auto maybe_peek_argument = m_lexer.peek_token();
        if (maybe_peek_argument.is_error())
            return maybe_peek_argument.release_error();
        if (maybe_peek_argument.value() != TokenType::Identifier) {
            return String::formatted("Expected identifier after keyword in message definition, got {}", token_to_string(maybe_peek_argument.value()));
        }

        auto maybe_argument = m_lexer.next_token();
        if (maybe_argument.is_error())
            return maybe_argument.release_error();

        auto argument = maybe_argument.release_value();
        arguments.append(argument.string());
    }

    if (name_builder.is_empty()) {
        VERIFY_NOT_REACHED();
    }

    auto maybe_peek_paren = m_lexer.peek_token();
    if (maybe_peek_paren.is_error())
        return maybe_peek_paren.release_error();
    if (maybe_peek_paren.value() != TokenType::ParenOpen)
        return String::formatted("Expected ( after message name, got {}", token_to_string(maybe_peek_paren.value()));
    m_lexer.consume_token();

    auto maybe_statement_list = parse_statement_list(false);
    if (maybe_statement_list.is_error())
        return maybe_statement_list.release_error();

    auto maybe_peek_paren_close = m_lexer.peek_token();
    if (maybe_peek_paren_close.is_error())
        return maybe_peek_paren_close.release_error();
    if (maybe_peek_paren_close.value() != TokenType::ParenClose)
        return String::formatted("Expected ) after statement list, got {}", token_to_string(maybe_peek_paren_close.value()));
    m_lexer.consume_token();

    return FunctionDefinitionNode::construct(name_builder.to_string(), move(arguments), maybe_statement_list.release_value());
}

Result<NonnullRefPtr<VariableAssignmentNode>, String> Parser::parse_variable_assignment()
{
    auto maybe_identifier = m_lexer.next_token();
    if (maybe_identifier.is_error())
        return maybe_identifier.release_error();
    auto identifier = maybe_identifier.release_value();
    if (identifier.type() != TokenType::Identifier)
        return String::formatted("Expected identifier at the start of variable assignment, got {}", token_to_string(identifier.type()));

    auto maybe_colon = m_lexer.peek_token();
    if (maybe_colon.is_error())
        return maybe_colon.release_error();
    if (maybe_colon.value() != TokenType::Colon)
        return String::formatted("Expected : after variable name, got {}", token_to_string(maybe_colon.value()));
    m_lexer.consume_token();

    auto maybe_expression = parse_expression();
    if (maybe_expression.is_error())
        return maybe_expression.release_error();

    return VariableAssignmentNode::construct(identifier.string(), maybe_expression.release_value());
}

Result<NonnullRefPtr<BlockNode>, String> Parser::parse_block()
{
    auto maybe_peek_bracket = m_lexer.peek_token();
    if (maybe_peek_bracket.is_error())
        return maybe_peek_bracket.release_error();
    if (maybe_peek_bracket.value() != TokenType::BracketOpen)
        return String::formatted("Expected [ at the start of block, got {}", token_to_string(maybe_peek_bracket.value()));
    m_lexer.consume_token();

    auto maybe_peek_pipe = m_lexer.peek_token();
    if (maybe_peek_pipe.is_error())
        return maybe_peek_pipe.release_error();

    Vector<String> arguments;

    if (maybe_peek_pipe.value() == TokenType::Pipe) {
        m_lexer.consume_token();

        while (true) {
            auto maybe_peek_closing_pipe = m_lexer.peek_token();
            if (maybe_peek_closing_pipe.is_error())
                return maybe_peek_closing_pipe.release_error();
            if (maybe_peek_closing_pipe.value() == TokenType::Pipe)
                break;

            auto maybe_identifier = m_lexer.next_token();
            if (maybe_identifier.is_error())
                return maybe_identifier.release_error();
            auto identifier = maybe_identifier.release_value();
            if (identifier.type() != TokenType::Identifier)
                return String::formatted("Expected identifier for block argument name, got {}", token_to_string(identifier.type()));

            arguments.append(identifier.string());

            auto maybe_peek_after_identifier = m_lexer.peek_token();
            if (maybe_peek_after_identifier.is_error())
                return maybe_peek_after_identifier.release_error();
            if (maybe_peek_after_identifier.value() == TokenType::Period) {
                m_lexer.consume_token();
            } else if (maybe_peek_after_identifier.value() != TokenType::Pipe) {
                return String::formatted("Expected . or | after block argument name, got {}", token_to_string(maybe_peek_after_identifier.value()));
            }
        }

        m_lexer.consume_token();
    }

    auto maybe_statement_list = parse_statement_list(false);
    if (maybe_statement_list.is_error())
        return maybe_statement_list.release_error();

    auto maybe_peek_after_block = m_lexer.peek_token();
    if (maybe_peek_after_block.is_error())
        return maybe_peek_after_block.release_error();
    if (maybe_peek_after_block.value() != TokenType::BracketClose)
        return String::formatted("Expected ] after block definition, got {}", token_to_string(maybe_peek_after_block.value()));
    m_lexer.consume_token();

    return BlockNode::construct(move(arguments), maybe_statement_list.release_value());
}
