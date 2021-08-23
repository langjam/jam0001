#include "named_enum.h"
#include "nettle.h"
#include "utils.h"
#include "nettleparse.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "ext/rxi/vec.h"

GENERATE_NAMED_ENUM_C(tokens, DEF_TOKENS);
GENERATE_LUT_TABLE_C(tokens, DEF_TOKENS);

bool is_type_single(int type)
{
    //return type == TOK_COMMENT_START || type == TOK_COMMENT_END || type == TOK_CODE_START || type == TOK_CODE_END || type == TOK_COLON || type == TOK_SEMICOLON || type == TOK_QUOTE_DOUBLE || type == TOK_QUOTE_SINGLE || type == TOK_COMMENT_DEFINITION;
    return 
        type == T_NPARENL ||
        type == T_NPARENR ||
        type == T_CPARENL ||
        type == T_CPARENR ||
        type == T_SPARENL || 
        type == T_SPARENR ||
        type == T_SEMICOLON ||
        type == T_HASH || 
        type == T_PLUS ||
        type == T_MINUS ||
        type == T_MULTIPLY ||
        type == T_DIVIDE ||
        type == T_XOR;
    
}
void nettle_error(struct nettle_file *ctx)
{
    fprintf(stderr, "nettle error: line %ld, position %ld: ", ctx->line_y+1, ctx->line_x+1);
    fflush(stderr);
}
void nettle_advance(struct nettle_file *ctx, long n)
{
    ctx->line_x += n;
    ctx->index += n;
}
int nettle_skip_newline(struct nettle_file *ctx)
{
    if (ctx->index >= ctx->length) { nettle_error(ctx); fprintf(stderr, "tried reading past eof\n"); return -1; }

    if (ctx->source[ctx->index] == 0x0d)
        nettle_advance(ctx, 1);
    if (ctx->index >= ctx->length) { nettle_error(ctx); fprintf(stderr, "malformed windows eol\n"); return -1; }

    if (ctx->source[ctx->index] == '\n')
        nettle_advance(ctx, 1);
    ctx->line_y++;
    ctx->line_x = 0;
    return 0;
}
struct { const char *name; int type; } token_name_pairs[] = {
    { "while", T_WHILE },
    { "if", T_IF },
    { "let", T_LET },
    { "return", T_RETURN },
    { "continue", T_CONTINUE },
    { "break", T_BREAK },
    { "elseif", T_ELSEIF },
};
long nettle_max(long a, long b)
{
    if (a < b)
        return a;
    return b;
}
void nettle_massage_identifier(struct nettle_file *ctx, struct nettle_token *token)
{
    if (token->type != T_IDENTIFIER)
        return;
    for (int i = 0; i < sizeof(token_name_pairs)/sizeof(token_name_pairs[0]); i++)
    {
        int len = strlen(token_name_pairs[i].name);
        if (token->multi.end-token->multi.start != len)
            continue;
        if (!strncmp(token_name_pairs[i].name, ctx->source + token->multi.start, len))
        {
            token->type = token_name_pairs[i].type;
            break;
        }
    }
    
}
int nettle_get_singlechar_type(char c)
{
    switch (c)
    {
        case '(': return T_NPARENL;
        case ')': return T_NPARENR;
        case '{': return T_CPARENL;
        case '}': return T_CPARENR;
        case '[': return T_SPARENL; 
        case ']': return T_SPARENR;
        case ';': return T_SEMICOLON;
        case '#': return T_HASH; 
        case '+': return T_PLUS;
        case '-': return T_MINUS;
        case '*': return T_MULTIPLY;
        case '/': return T_DIVIDE;
        case '^': return T_XOR;
        default:  return T_EMPTY;
    }        
}
void nettle_tokenize(struct nettle_file *ctx, vec_nettle_token_t *token_buffer)
{
    #define FLUSH_GLOBAL() \
        do { \
            if (tok_global.type != T_EMPTY) \
            { \
                nettle_massage_identifier(ctx, &tok_global); \
                vec_push(token_buffer, tok_global); \
                memset(&tok_global, 0, sizeof(struct nettle_token)); \
            } \
        } while(0)
    struct nettle_token tok_global = {0};
    bool multi_found = false;
    char multi_first_char = ' ';
    int comment_depth = 0;
    while (1)
    {
        char c;
        if (ctx->index >= ctx->length)
            break;
        c = ctx->source[ctx->index];

        if (multi_found)
        {
            struct nettle_token tok = {0};
            tok.multi.start = ctx->index - 1;
            multi_found = false;
            switch (multi_first_char)
            {
                case '<': {
                    if (c == '<')
                    {
                        tok.type = T_LSHIFT;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else if (c == '=') 
                    {
                        tok.type = T_LETHAN;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else {
                        tok.type = T_LTHAN;
                        tok.multi.end = ctx->index;
                        vec_push(token_buffer, tok);
                    }
                } break;
                case '>': {
                    if (c == '>')
                    {
                        tok.type = T_RSHIFT;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else if (c == '=') 
                    {
                        tok.type = T_GETHAN;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else {
                        tok.type = T_GTHAN;
                        tok.multi.end = ctx->index;
                        vec_push(token_buffer, tok);
                    }
                } break;
                case '!': {
                    if (c == '=')
                    {
                        tok.type = T_INEQUAL;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else {
                        tok.type = T_NOT;
                        tok.multi.end = ctx->index;
                        vec_push(token_buffer, tok);
                    }
                } break;
                case '&': {
                    if (c == '&')
                    {
                        tok.type = T_LAND;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else {
                        tok.type = T_BAND;
                        tok.multi.end = ctx->index;
                        vec_push(token_buffer, tok);
                    }
                } break;
                case '|': {
                    if (c == '|')
                    {
                        tok.type = T_LOR;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else {
                        tok.type = T_BOR;
                        tok.multi.end = ctx->index;
                        vec_push(token_buffer, tok);
                    }
                } break;
                case ':': {
                    if (c == '=')
                    {
                        tok.type = T_ASSIGN;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                        continue;
                    } else {
                        tok.type = T_COLON;
                        tok.multi.end = ctx->index;
                        vec_push(token_buffer, tok);
                    }
                } break;
                case '=': {
                    if (c == '=')
                    {
                        tok.type = T_EQUAL;
                        tok.multi.end = ctx->index+1;
                        vec_push(token_buffer, tok);
                        nettle_advance(ctx, 1);
                    } else {
                        nettle_error(ctx);
                        fprintf(stderr, "lone equal sign\n");
                        return;
                    }
                } break;
            }

        }
        c = ctx->source[ctx->index];
        if (c == '#' || c == ';' || c == '{' || c == '}' || c == '(' || c == ')' ||c == '[' || c == ']' || c == '+' || c == '-' || c == '*' || c == '/' || c == '^')
        {
            struct nettle_token tok = {0};
            FLUSH_GLOBAL();
            tok.type = nettle_get_singlechar_type(c);
            tok.single.position = ctx->index;

            vec_push(token_buffer, tok);
            nettle_advance(ctx, 1);
            if (c == '{' || (comment_depth && c == ';'))
            {
                if ( c == '{')
                    comment_depth++;
                while (1)
                {
                    if (ctx->index >= ctx->length)
                    {
                        nettle_error(ctx);
                        fprintf(stderr, "tried reading past eof\n");
                        return;
                    }
                    c = ctx->source[ctx->index];
                    if (is_whitespace(c))
                    {
                        nettle_advance(ctx, 1);
                        continue;
                    } else if (is_newline(c))
                    {
                        tok_global.multi.end = ctx->index;
                        FLUSH_GLOBAL();
                        nettle_skip_newline(ctx);
                    } else if (c == '#' || c == '}')
                    {
                        tok_global.multi.end = ctx->index;
                        FLUSH_GLOBAL();
                        if ( c == '}')
                            comment_depth--;
                        break;
                    } else {
                        if (tok_global.type == T_EMPTY)
                        {
                            tok_global.type = T_COMMENT;
                            tok_global.multi.start = ctx->index;
                        }
                        nettle_advance(ctx, 1);
                    }
                }
                continue;
            }
        } else if (!multi_found && (c == '<' || c == '>' || c == '!' || c == '&' || c == '|' || c == ':' || c == '=')) {
            FLUSH_GLOBAL();
            multi_found = true;
            multi_first_char = c;
            nettle_advance(ctx, 1);
        } else if (c == '"')
        {
            struct nettle_token tok = {0};
            FLUSH_GLOBAL();
            tok.type = T_STRING;
            tok.multi.start = ctx->index + 1;
            nettle_advance(ctx, 1);
            c = ctx->source[ctx->index];
            while (1)
            {
                if (ctx->index >= ctx->length || is_newline(c))
                {
                    nettle_error(ctx);
                    fprintf(stderr, "non ended string\n");
                    return;
                }

                c = ctx->source[ctx->index];
                if (c == '"')
                    break;
                
                nettle_advance(ctx, 1);
            }
            tok.multi.end = ctx->index;
            vec_push(token_buffer, tok);

            nettle_advance(ctx, 1);
        } else if (is_dec(c))
        {
            FLUSH_GLOBAL();
            long index = 0;
            int base = 10;
            char first_digit = c;

            struct nettle_token tok = {0};
            tok.type = T_INTEGER;
            long start, end;
            start = ctx->index;
            while (1)
            {
                if (ctx->index >= ctx->length)
                {
                    nettle_error(ctx);
                    fprintf(stderr, "tried reading number past eof\n");
                    return;
                }
                c = ctx->source[ctx->index];
                if (index == 1 && first_digit == '0')
                {
                    if (c == 'x') { base = 16; }
                    else if (c == 'o') { base = 8; }
                    else if (c == 'b') { base = 2; }
                    else if (is_dec(c)) { base = 10; }
                    else {
                        break;
                    }
                    start += 2;
                    nettle_advance(ctx, 1);
                    index++;
                    continue;
                }

                // how the h do you format this to look nice
                if ( (base == 16 && !is_hex(c)) ||
                     (base == 10 && !is_dec(c)) ||
                     (base == 8  && !is_oct(c)) ||
                     (base == 2  && !is_bin(c)) )
                {
                    break;
                }
                index++;
                nettle_advance(ctx, 1);
            }
            end = ctx->index;
            // parse
            char *endp = ctx->source + end;
            long value = strtol(ctx->source + start, &endp, base);
            tok.lit_number.value = value;
            vec_push(token_buffer, tok);
        } else if (tok_global.type == T_EMPTY && is_identifier(c, 0)) {
            tok_global.type = T_IDENTIFIER;
            tok_global.multi.start = ctx->index;
            tok_global.multi.end = ctx->index + 1;
            nettle_advance(ctx, 1);
        } else if (tok_global.type == T_IDENTIFIER && is_identifier(c, tok_global.multi.end-tok_global.multi.start+1)) {
            tok_global.multi.end = ctx->index+1;
            nettle_advance(ctx, 1);
        } else if (is_whitespace(c))
        {
            FLUSH_GLOBAL();
            nettle_advance(ctx, 1);
        } else if (is_newline(c))
        {
            FLUSH_GLOBAL();
            nettle_skip_newline(ctx);
        }
    }
}
typedef void *ParseAllocFunc_t(size_t);
typedef void ParseFreeFunc_t(void*);

extern void *ParseAlloc(ParseAllocFunc_t *func);
extern void Parse(
    void *pParser, 
    int tokenCode, 
    struct nettle_parser_token *token, 
    struct nettle_parser_state *state
);
extern void ParseFree(void *pParser, ParseFreeFunc_t *freeFunc);
extern void ParseTrace(FILE *stream, char *zPrefix);

void nettle_parse(struct nettle_file *ctx, vec_nettle_token_t *token_buffer)
{
    void *parser = ParseAlloc(malloc);
    if (!parser)
    {
        fprintf(stderr, "failed to allocate parser\n");
        return;
    }
    //ParseTrace(stdout, "pref ");
    struct nettle_parser_state state = {0};
    struct nettle_token token;
    int i = 0;

    vec_init(&state.global_comment_blocks);
    vec_foreach(token_buffer, token, i) {
        char *ident_name = NULL;
        struct nettle_parser_token *param = malloc(sizeof(struct nettle_parser_token));
        if (tokens_lut[token.type] == 0)
            continue;

        if (token.type == T_IDENTIFIER || token.type == T_STRING)
        {
            ident_name = malloc(token.multi.end - token.multi.start + 1);
            memcpy(ident_name, ctx->source + token.multi.start, token.multi.end - token.multi.start);
            ident_name[token.multi.end - token.multi.start] = 0;
        }

        switch(token.type)
        {

            case T_PLUS:     { param->type = PT_OPERATOR; param->operator.type = '+'; } break;
            case T_MINUS:    { param->type = PT_OPERATOR; param->operator.type = '-'; } break;
            case T_MULTIPLY: { param->type = PT_OPERATOR; param->operator.type = '*'; } break;
            case T_DIVIDE:   { param->type = PT_OPERATOR; param->operator.type = '/'; } break;
            case T_MODULO:   { param->type = PT_OPERATOR; param->operator.type = '%'; } break;
            case T_LTHAN:    { param->type = PT_OPERATOR; param->operator.type = '<'; } break;
            case T_GTHAN:    { param->type = PT_OPERATOR; param->operator.type = '>'; } break;
            case T_LETHAN:   { param->type = PT_OPERATOR; param->operator.type = 'S'; } break;
            case T_GETHAN:   { param->type = PT_OPERATOR; param->operator.type = 'G'; } break;
            case T_EQUAL:    { param->type = PT_OPERATOR; param->operator.type = '='; } break;
            case T_INEQUAL:  { param->type = PT_OPERATOR; param->operator.type = '!'; } break;
            case T_LSHIFT:   { param->type = PT_OPERATOR; param->operator.type = 'L'; } break;
            case T_RSHIFT:   { param->type = PT_OPERATOR; param->operator.type = 'R'; } break;
            case T_LAND:     { param->type = PT_OPERATOR; param->operator.type = 'A'; } break;
            case T_LOR:      { param->type = PT_OPERATOR; param->operator.type = 'O'; } break;
            case T_NOT:      { param->type = PT_OPERATOR; param->operator.type = 'N'; } break;
            case T_BAND:     { param->type = PT_OPERATOR; param->operator.type = '&'; } break;
            case T_BOR:      { param->type = PT_OPERATOR; param->operator.type = '|'; } break;
            case T_XOR:      { param->type = PT_OPERATOR; param->operator.type = '^'; } break;

            case T_INTEGER: {
                param->type = PT_NUMBER;
                param->number.value = token.lit_number.value;
            } break;
            case T_IDENTIFIER: {
                param->type = PT_IDENTIFIER;
                param->identifier.value = ident_name;
            } break;
            case T_STRING: {
                param->type = PT_STRING;
                param->string.value = ident_name;
            } break;
        }
        Parse(parser, tokens_lut[token.type], param, &state);
        
        //if (ident_name)
        //    free(ident_name);
    }
    Parse(parser, 0, NULL, &state);
    ParseFree(parser, free);
    struct nettle_parser_comment_block *block;
    i = 0;
    vec_foreach(&state.global_comment_blocks, block, i)
    {
        printf("global block %d\n", i);
    }

    vec_deinit(&state.global_comment_blocks);
}

/* comment block list */
/* main function */
void nettle(char *src, long src_length)
{
    struct nettle_file file = {0};
    vec_nettle_token_t token_buffer = {0};
    int ret = 1;
    file.length = src_length;
    file.source = src;

    vec_init(&token_buffer);
    printf("lexing...\n");
    nettle_tokenize(&file, &token_buffer);
/*
    struct nettle_token token;
    int i;
    vec_foreach(&token_buffer, token, i) {
        printf("+ %s (%.*s)\n", tokens_names[token.type], ( is_type_single(token.type) ? 1 : token.multi.end-token.multi.start), src + token.multi.start);
    }
*/  
    /* REMOVED FOR SAFETY */
    //nettle_parse(&file, &token_buffer);
    vec_deinit(&token_buffer);

}
