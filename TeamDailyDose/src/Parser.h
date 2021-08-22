#pragma once

#include "AST.h"
#include "Lexer.h"

class Parser {
public:
    Parser(Lexer& lexer)
        : m_lexer(lexer)
    {
    }

    Result<NonnullRefPtr<StatementListNode>, String> parse_statement_list(bool toplevel = false);
    Result<NonnullRefPtr<ASTNode>, String> parse_statement(bool toplevel = false);
    Result<NonnullRefPtr<ASTNode>, String> parse_expression();
    Result<NonnullRefPtr<ASTNode>, String> parse_primary();
    Result<NonnullRefPtr<CommentNode>, String> parse_comment();
    Result<NonnullRefPtr<IdentifierNode>, String> parse_identifier();
    Result<NonnullRefPtr<ASTNode>, String> parse_message_like(NonnullRefPtr<CommentNode>);
    Result<NonnullRefPtr<FunctionDefinitionNode>, String> parse_function_definition();
    Result<NonnullRefPtr<VariableAssignmentNode>, String> parse_variable_assignment();
    Result<NonnullRefPtr<BlockNode>, String> parse_block();

    Location const& location() const { return m_lexer.location(); }

private:
    Lexer& m_lexer;
};
