#pragma once

#include "ast.h"
#include "lexer.h"
#include <AK/Queue.h>

struct ParseError {
    String error;
    Token::Position where;
};

class Parser {
public:
    explicit Parser(Lexer& lexer)
        : m_lexer(lexer)
    {
    }

    Result<Vector<NonnullRefPtr<ASTNode>>, ParseError> parse_toplevel(bool for_func = false);

private:
    Token peek()
    {
        if (m_unconsumed_tokens.is_empty()) {
            auto next = m_lexer.next();
            m_unconsumed_tokens.enqueue(next.release_value());
        }
        return m_unconsumed_tokens.head();
    }

    Result<Token, LexError> consume()
    {
        if (m_unconsumed_tokens.is_empty())
            return m_lexer.next();

        return m_unconsumed_tokens.dequeue();
    }

    static ParseError error(LexError error) { return { move(error.error), error.where }; }

    Result<NonnullRefPtr<ASTNode>, ParseError> parse_assignment();
    Result<NonnullRefPtr<ASTNode>, ParseError> parse_expression();
    Result<NonnullRefPtr<ASTNode>, ParseError> parse_literal();
    Result<NonnullRefPtr<ASTNode>, ParseError> parse_mention(bool is_direct = true);
    Result<NonnullRefPtr<ASTNode>, ParseError> parse_function();
    Result<NonnullRefPtr<ASTNode>, ParseError> parse_call(NonnullRefPtr<ASTNode>);
    Result<NonnullRefPtr<Variable>, ParseError> parse_variable();
    Result<NonnullRefPtr<ASTNode>, ParseError> parse_record_decl();
    Result<NonnullRefPtr<ASTNode>, ParseError> parse_member_access(NonnullRefPtr<ASTNode>);

    ParseError make_error_here(String);

    Lexer& m_lexer;
    Queue<Token> m_unconsumed_tokens;
};

[[maybe_unused]] constexpr auto the_grammar = R"~~~(
toplevel :: ((assignment | expression) Semicolon)*

assignment :: Identifier("let") variable Equals expression

expression :: literal
            | mention
            | function
            | call
            | variable
            | record_decl
            | OpenParen expression CloseParen
            | Comment
            | member_access

literal :: IntegerLiteral
         | StringLiteral

mention :: direct_mention
         | indirect_mention

function :: OpenBrace parameters? return? (expression Semicolon)* CloseBrace

call :: expression OpenParen (expression*) CloseParen

variable :: Identifier (Colon expression)?

record_decl :: Identifier("record") OpenBrace record_contents CloseBrace

member_access :: member_access Dot Identifier

direct_mention :: OpenMention Identifier+ CloseMention

indirect_mention :: OpenIndirectMention expression CloseMention

parameters :: Pipe variable* Pipe

return :: Colon variable

record_contents :: (variable Comma)*
)~~~";
