%option noyywrap

%{

#include "parser.h"
#include "parser.tab.h"
#include "allocator.h"
#include "base64.h"
#include <string.h>
#include <math.h>

const char *eval_escapes(const char *str)
{
    char *buffer = malloc(strlen(str) + 1);
    int buffer_pointer = 0;

    bool is_in_escape = false;
    for (const char *c = str; *c; c++)
    {
        if (is_in_escape)
        {
            switch (*c)
            {
                case 'n': buffer[buffer_pointer++] = '\n'; break;
                case 'r': buffer[buffer_pointer++] = '\r'; break;
                case 't': buffer[buffer_pointer++] = '\t'; break;
                case '\\': buffer[buffer_pointer++] = '\\'; break;
                default: buffer[buffer_pointer++] = *c; break;
            }
            is_in_escape = false;
        }
        else
        {
            if (*c == '\\')
                is_in_escape = true;
            else
                buffer[buffer_pointer++] = *c;
        }
    }

    buffer[buffer_pointer] = '\0';
    return buffer;
}

%}

%option yylineno

%%

[ \t\n]     ;

"none"      { yylval.datatype = DATA_TYPE_NONE; return TOKEN_DATA_TYPE; }
"number"    { yylval.datatype = DATA_TYPE_NUMBER; return TOKEN_DATA_TYPE; }
"string"    { yylval.datatype = DATA_TYPE_STRING; return TOKEN_DATA_TYPE; }
"list"      { yylval.datatype = DATA_TYPE_LIST; return TOKEN_DATA_TYPE; }

"/dev/null" { yylval.number = NAN; return TOKEN_NUMBER; }
"None"      { yylval.value.type = VALUE_TYPE_NONE; return TOKEN_VALUE; }
"match"     { return TOKEN_MATCH; }
"eval"      { return TOKEN_EVAL; }
"define"    { return TOKEN_DEFINE; }
"please"    { return TOKEN_PLEASE; }
"thanks"    { return TOKEN_THANKS; }

"->"        { return TOKEN_ARROW; }
"{"         { return TOKEN_OPEN_SQUIGLE; }
"}"         { return TOKEN_CLOSE_SQUIGLE; }
"["         { return TOKEN_OPEN_SQUARE; }
"]"         { return TOKEN_CLOSE_SQUARE; }
"("         { return TOKEN_OPEN_BRACE; }
")"         { return TOKEN_CLOSE_BRACE; }
","         { return TOKEN_COMMA; }
":"         { return TOKEN_COLON; }

"<="        { yylval.comparitor = COMPARITOR_LESS_THAN_OR_EQUAL; return TOKEN_COMPARITOR; }
">="        { yylval.comparitor = COMPARITOR_MORE_THAN_OR_EQUAL; return TOKEN_COMPARITOR; }
"<"         { yylval.comparitor = COMPARITOR_LESS_THAN; return TOKEN_COMPARITOR; }
">"         { yylval.comparitor = COMPARITOR_MORE_THAN; return TOKEN_COMPARITOR; }
"="         { yylval.comparitor = COMPARITOR_EQUALS; return TOKEN_COMPARITOR; }

-?[a-zA-Z0-9_$.]+ { 
    yylval.number = base64_decode_double(yytext);
    return TOKEN_NUMBER; 
}

"+"         { yylval.op = OPERATOR_ADD; return TOKEN_OPERATOR; }
"-"         { yylval.op = OPERATOR_SUBTRACT; return TOKEN_OPERATOR; }
"*"         { yylval.op = OPERATOR_MULTIPLY; return TOKEN_OPERATOR; }
"/"         { yylval.op = OPERATOR_DIVIDE; return TOKEN_OPERATOR; }

\"[^\"]*\"  { 
    const char *str = eval_escapes(yytext);
    yylval.value.type = VALUE_TYPE_STRING;
    yylval.value.str = allocator_sub_string(str, 1, 1);
    free(str);
    return TOKEN_VALUE;
}

%%

bool did_error = false;

void yyerror(const char *err)
{
    fprintf(stderr, "Error [%d]: '%s'\n", yylineno, err);
    did_error = true;
}

