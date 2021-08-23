#include "../lib/test.h"
#include "../../aid/vec/vec.h"
#include "../../aid/common/prelude.h"
#include "../../lang/lexer.h"

static bool try_lex(const string input, enum Token_Type /* expected token type */ xtt, const char * /*expected string */ xs)
{
    struct Lexer_State lexer = lex_new(input);
    struct Token t = lex_determine(&lexer);
    char tmp[4096] = { 0 };
    lex_drop(&lexer);
    INFO_MANUAL(
        printf(
            "Checking `%s` against `%s` [%s], actual result is ",
            input, xs, TT_NAMES[xtt]
        );
        snprintf(tmp, t.span.size+1, "%s", input+t.span.from);
        printf("`%s`", tmp);
        printf(" [%s]", TT_NAMES[t.tt]);
    );
    bool cond = t.tt == xtt;
    if (xs != NULL)
        cond = cond && spanstreqstr(t.span, input, xs);
    return cond;
}

void test_lexer() 
{
    TEST_BEGIN;
    EXPECT(try_lex("", TT_EOF, ""));
    EXPECT(try_lex("п", TT_INVALID, NULL));
    EXPECT(try_lex(" : ", TT_DEF, ":"));
    EXPECT(try_lex(" = ", TT_ASSIGN, "="));
    EXPECT(try_lex(" \"I'm really cool \\\" string\" ", TT_STRING, "\"I'm really cool \\\" string\""));
    EXPECT(try_lex(" 12312 ", TT_NUMBER, "12312"));
    EXPECT(try_lex(" -12312 ", TT_NUMBER, "-12312"));
    EXPECT(try_lex(" test_ident ", TT_IDENT, "test_ident"));
    EXPECT(try_lex(" Test_Ident ", TT_IDENT, "Test_Ident"));
    EXPECT(try_lex(" test_ident123 ", TT_IDENT, "test_ident123"));
    EXPECT(try_lex(" Test_Ident321 ", TT_IDENT, "Test_Ident321"));
    EXPECT(try_lex(" ( ", TT_LPAREN, "("));
    EXPECT(try_lex(" ) ", TT_RPAREN, ")"));
    EXPECT(try_lex(" { ", TT_LBRACE, "{"));
    EXPECT(try_lex(" } ", TT_RBRACE, "}"));
    EXPECT(try_lex(" [ ", TT_LBRACKET, "["));
    EXPECT(try_lex(" ] ", TT_RBRACKET, "]"));
    EXPECT(try_lex(" ; ", TT_SEMI, ";"));
    EXPECT(try_lex(" return ", TT_RETURN, "return"));
}
