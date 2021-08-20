#pragma once

#include "lexer.h"
#include <AK/HashMap.h>

class ASTNode {
};

class Parser {
public:
    explicit Parser(Lexer& lexer)
        : m_lexer(lexer)
    {
    }

private:
    Lexer& m_lexer;
};

[[maybe_unused]] constexpr auto the_grammar = R"~~~(
expression :: literal
            | mention
            | function
            | call
            | variable
            | record_decl
            | OpenParen expression CloseParen
            | Comment

literal :: IntegerLiteral
         | StringLiteral

mention :: direct_mention
         | indirect_mention

function :: OpenBrace parameters? return? (expression Semicolon)* CloseBrace

call :: expression OpenParen arguments? CloseParen

variable :: Identifier (Colon expression)?

record_decl :: Identifier("record") OpenBrace record_contents CloseBrace

direct_mention :: OpenMention Identifier+ CloseMention

indirect_mention :: OpenIndirectMention expression CloseMention

parameters :: Pipe (variable (, variable)*)? Pipe

return :: Colon variable

record_contents :: (variable Comma)*
)~~~";
