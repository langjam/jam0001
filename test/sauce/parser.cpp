#include "parser.h"
#include <AK/TypeCasts.h>

Result<Vector<NonnullRefPtr<ASTNode>>, ParseError> Parser::parse_toplevel()
{
    Vector<NonnullRefPtr<ASTNode>> nodes;
    for (;;) {
        auto maybe_token = consume();
        if (maybe_token.is_error())
            return error(maybe_token.release_error());

        auto token = maybe_token.release_value();
        if (token.type == Token::Type::Eof)
            break;

        m_unconsumed_tokens.enqueue(token);

        Optional<Result<NonnullRefPtr<ASTNode>, ParseError>> maybe_node;
        if (token.type == Token::Type::Identifier && token.text == "let"sv)
            maybe_node = parse_assignment();
        else
            maybe_node = parse_expression();

        if (maybe_node->is_error())
            return maybe_node->release_error();

        if (peek().type == Token::Type::Semicolon)
            (void)consume();

        nodes.append(static_ptr_cast<ASTNode>(create<Statement>(maybe_node->release_value())));
    }

    return nodes;
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_expression()
{
    auto parse_primary = [this]() -> Result<NonnullRefPtr<ASTNode>, ParseError> {
        switch (peek().type) {
        case Token::Type::Unknown:
            outln("invalid token starting at {}@{}:{} '{}'", to_underlying(peek().type), peek().source_range.start.line, peek().source_range.start.column, peek().text);
            return make_error_here("Unexpected invalid token");
        case Token::Type::Identifier: {
            if (peek().text == "record"sv)
                return parse_record_decl();

            auto var = parse_variable();
            if (var.is_error())
                return var.release_error();
            return static_ptr_cast<ASTNode>(var.release_value());
        }
        case Token::Type::Comment:
            return static_ptr_cast<ASTNode>(create<Comment>(consume().release_value().text));
        case Token::Type::MentionOpen:
            return parse_mention();
        case Token::Type::MentionClose:
            return make_error_here("Unexpected '>'");
        case Token::Type::IndirectMentionOpen:
            TODO();
            break;
        case Token::Type::OpenBrace:
            return parse_function();
        case Token::Type::CloseBrace:
            return make_error_here("Unexpected '}'");
        case Token::Type::OpenParen: {
            (void)consume();
            auto expression = parse_expression();
            if (expression.is_error())
                return expression.release_error();
            auto res = consume();
            if (res.is_error())
                return error(res.release_error());

            if (res.value().type != Token::Type::CloseParen)
                return make_error_here("Expected a close paren");

            return expression;
        }
        case Token::Type::CloseParen:
            return make_error_here("Unexpected ')'");
        case Token::Type::Comma:
            return make_error_here("Unexpected ','");
        case Token::Type::Colon:
            return make_error_here("Unexpected ':'");
        case Token::Type::Semicolon:
            (void)consume();
            return parse_expression();
        case Token::Type::String:
        case Token::Type::Integer:
            return parse_literal();
        case Token::Type::Pipe:
            return make_error_here("Unexpected '|'");
        case Token::Type::Equals:
            return make_error_here("Unexpected '='");
        case Token::Type::Dot:
            return make_error_here("Unexpected '.'");
        case Token::Type::Eof:
            return make_error_here("Unexpected eof");
        }
    };
    Optional<Result<NonnullRefPtr<ASTNode>, ParseError>> primary = parse_primary();
    if (primary->is_error())
        return primary->error();

    if (is<Comment>(*primary->value()))
        return *primary;

    while (peek().type != Token::Type::Semicolon) {
        if (peek().type == Token::Type::Eof)
            return *primary;

        if (peek().type == Token::Type::OpenParen) {
            primary = parse_call(primary->value());
            if (primary->is_error())
                return primary->release_error();
            continue;
        }

        return *primary;
    }

    return *primary;
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_assignment()
{
    [[maybe_unused]] auto skipped_value = consume();
    auto var = parse_variable();
    if (var.is_error())
        return var.release_error();
    auto equals = consume();
    if (equals.is_error())
        return error(equals.release_error());
    if (equals.value().type != Token::Type::Equals)
        return make_error_here("Expected '='");
    auto expr = parse_expression();
    if (expr.is_error())
        return expr.release_error();

    return static_ptr_cast<ASTNode>(create<Assignment>(var.release_value(), expr.release_value()));
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_literal()
{
    if (peek().type == Token::Type::String)
        return static_ptr_cast<ASTNode>(create<StringLiteral>(consume().release_value().text));

    auto token = consume().release_value();
    VERIFY(token.type == Token::Type::Integer);

    auto value = token.text.to_int();
    if (!value.has_value())
        return make_error_here("Invalid integer value");

    return static_ptr_cast<ASTNode>(create<IntegerLiteral>(*value));
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_mention()
{
    (void)consume();
    Vector<String> queries;
    while (peek().type != Token::Type::MentionClose) {
        if (peek().type == Token::Type::Eof)
            return make_error_here("Expected a '>', but reached eof");

        auto token = consume();
        if (token.is_error())
            return error(token.release_error());

        if (token.value().type != Token::Type::Identifier)
            return make_error_here("Expected an identifier");

        queries.append(token.release_value().text);
    }

    auto close_type = consume().release_value().type;
    VERIFY(close_type == Token::Type::MentionClose);
    return static_ptr_cast<ASTNode>(create<DirectMention>(move(queries)));
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_function()
{
    return make_error_here("Unimplemented: function");
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_call(NonnullRefPtr<ASTNode> callee)
{
    auto open_paren_type = consume().release_value().type;
    VERIFY(open_paren_type == Token::Type::OpenParen);

    Vector<AK::NonnullRefPtr<ASTNode>> arguments;
    while (peek().type != Token::Type::CloseParen) {
        if (peek().type == Token::Type::Eof)
            return make_error_here("Expected a close paren");

        auto expr = parse_expression();
        if (expr.is_error())
            return expr.release_error();

        arguments.append(expr.release_value());
    }

    (void)consume();
    return static_ptr_cast<ASTNode>(create<Call>(move(callee), move(arguments)));
}

Result<NonnullRefPtr<Variable>, ParseError> Parser::parse_variable()
{
    auto ident = consume().release_value();
    VERIFY(ident.type == Token::Type::Identifier);
    auto maybe_token = consume();
    if (maybe_token.is_error())
        return error(maybe_token.release_error());

    RefPtr<ASTNode> type;
    if (maybe_token.value().type == Token::Type::Colon) {
        auto type_expr = parse_expression();
        if (type_expr.is_error())
            return type_expr.release_error();
        type = type_expr.release_value();
    } else {
        if (maybe_token.value().type != Token::Type::Eof)
            m_unconsumed_tokens.enqueue(maybe_token.release_value());
    }
    return create<Variable>(move(ident.text), move(type));
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_record_decl()
{
    return make_error_here("Unimplemented: record");
}

Result<NonnullRefPtr<ASTNode>, ParseError> Parser::parse_member_access()
{
    return make_error_here("Unimplemented: member");
}

ParseError Parser::make_error_here(String text)
{
    return {
        move(text),
        m_lexer.current_source_position()
    };
}
