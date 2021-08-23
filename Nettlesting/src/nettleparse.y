%token_type { struct nettle_parser_token* }
%token_prefix YT_

%token NPARENL.
%token NPARENR.
%token CPARENL.
%token CPARENR.
%token SPARENL.
%token SPARENR.
%token SEMICOLON.
%token HASH.
%token PLUS.
%token MINUS.
%token MULTIPLY.
%token DIVIDE.
%token MODULO.
%token XOR.
%token COLON.
%token EQUAL.
%token INEQUAL.
%token NOT.
%token GTHAN.
%token LTHAN.
%token GETHAN.
%token LETHAN.
%token LSHIFT.
%token RSHIFT.
%token LAND.
%token LOR.
%token BAND.
%token BOR.
%token ASSIGN.
%token STRING.
%token INTEGER.
%token IDENTIFIER.
%token COMMENT.
%token WHILE.
%token IF.
%token LET.
%token RETURN.
%token CONTINUE.
%token ELSEIF.
%token BREAK.

%left LOR.
%left LAND. 
%left BOR.
%left XOR. 
%left BAND . 
%left INEQUAL EQUAL.
%left GTHAN LTHAN LETHAN GETHAN.
%left LSHIFT RSHIFT.
%left MINUS PLUS.
%left MULTIPLY DIVIDE MODULO.
%right POSITIVE NEGATIVE NOT.

%start_symbol program
%extra_argument { struct nettle_parser_state* GLOBAL_STATE}

%type expression { struct nettle_parser_expression* }
%type operator { struct nettle_parser_token* }
%type comment_definition { struct nettle_parser_comment_block_definition* }
%type comment_definitions { vec_nettle_parser_comment_block_definition_t* }
%type comment_block { struct nettle_parser_comment_block* }
%type code_block { struct nettle_parser_code_block* }
%type code_statements { vec_nettle_parser_statement_t* }
%type code_statement { struct nettle_parser_statement* }
%type if_statement { struct nettle_if_statement* }
%type elseif_statement { struct nettle_if_statement* }
%type while_statement { struct nettle_while_statement* }

%include {
    #include <stdio.h>
    #include <stdlib.h>
    #include "nettle.h"
    #include "ext/sheredom/hashmap.h"
    #include "ext/rxi/vec.h"
}
%syntax_error {
    printf("syntax error\n");
}

program ::= statements.
statements ::= statements statement.
statements ::= statement.

statement ::= comment_block(B) LETHAN code_block. {
    B->flags = CMFLG_BOUND;
    vec_push(&GLOBAL_STATE->global_comment_blocks, B);
    printf("found bound global comment block\n");
}
statement ::= comment_block(B). {
    B->flags = CMFLG_UNBOUND;
    vec_push(&GLOBAL_STATE->global_comment_blocks, B);
    printf("found unbound global comment block\n");
}

// comment block
comment_block(B) ::= CPARENL comment_definitions(D) CPARENR. { 
    //if (!B)
    //    B = malloc(sizeof(struct nettle_parser_comment_block));
    B->flags = CMFLG_UNBOUND;
    if (!B->map)
    {
        B->map = malloc(sizeof(struct hashmap_s));
        hashmap_create(32, B->map);
    }
    struct nettle_parser_comment_block_definition *def;
    int i = 0;
    printf("found comment with definitions\n");
    vec_foreach(D, def, i) {
        hashmap_put(B->map, def->name, strlen(def->name), def->expression_tree);
        printf(" %s\n", def->name);
    }
}
comment_block ::= CPARENL comment CPARENR. { /* ignore */ }
comment_block ::= CPARENL CPARENR. { /* ignore */ }

comment ::= comment COMMENT.
comment ::= COMMENT.

comment_definitions(V) ::= comment_definitions comment_definition(D). { if (!V) {V = malloc(sizeof(vec_nettle_parser_comment_block_definition_t)); vec_init(V);} vec_push(V, D); }
comment_definitions(V) ::= comment comment_definition(D).             { if (!V) {V = malloc(sizeof(vec_nettle_parser_comment_block_definition_t)); vec_init(V);} vec_push(V, D); }
comment_definitions(V) ::= comment_definition(D) comment.             { if (!V) {V = malloc(sizeof(vec_nettle_parser_comment_block_definition_t)); vec_init(V);} vec_push(V, D); }
comment_definitions(V) ::= comment_definition(D).                     { if (!V) {V = malloc(sizeof(vec_nettle_parser_comment_block_definition_t)); vec_init(V);} vec_push(V, D); }

comment_definition(D) ::= HASH IDENTIFIER(ID) COLON comment_block SEMICOLON. { 
    D->name = ID->identifier.value;
    free(ID);
    printf("found comment definition with comment block\n"); 
}
comment_definition(D) ::= HASH IDENTIFIER(ID) COLON expression(E) SEMICOLON. {
    D->name = ID->identifier.value;
    free(ID);
    D->expression_tree = E;
}

// code block parsing
code_block ::= SPARENL code_statements SPARENR.
code_block ::= SPARENL SPARENR.

code_statements(D) ::= code_statements(A) code_statement(B). {
    if (A && B)
    {
        if (!D)
        {
            D = malloc(sizeof(vec_nettle_parser_statement_t)); 
            vec_init(D); 
        }
        vec_extend(D, A);
        vec_push(D, B); 
        vec_deinit(A);
        free(B);
    }
}
code_statements ::= code_statement. { 
}

code_statement ::= comment_block LETHAN if_statement SEMICOLON. { printf("bind if\n"); }
code_statement ::= comment_block LETHAN while_statement SEMICOLON. { printf("bind while\n"); }
code_statement ::= comment_block LETHAN code_block SEMICOLON. { printf("bind code\n"); }
code_statement ::= comment_block LETHAN expression(E) SEMICOLON. { printf("bind expr %p\n", E); }
code_statement ::= comment_block SEMICOLON. {
    // figure something out here
}

code_statement(S) ::= if_statement(I) SEMICOLON. { 
    S = malloc(sizeof(struct nettle_parser_statement));
    S->type = STMT_IF;
    S->data_if = I;
}
code_statement(S) ::= while_statement(W) SEMICOLON. { 
    S = malloc(sizeof(struct nettle_parser_statement));
    S->type = STMT_WHILE;
    S->data_while = W;
}
code_statement ::= comment_block.

code_statement ::= HASH IDENTIFIER LETHAN comment_block SEMICOLON. { printf("call params\n"); }
code_statement ::= HASH IDENTIFIER SEMICOLON. { printf("call\n"); }

code_statement ::= LET IDENTIFIER ASSIGN expression SEMICOLON. { printf("let expression\n"); }
code_statement ::= RETURN SEMICOLON. { printf("return\n"); }
code_statement ::= CONTINUE IDENTIFIER SEMICOLON. { printf("continue\n"); }
code_statement ::= BREAK IDENTIFIER SEMICOLON. { printf("break\n"); }

if_statement(S) ::= IF expression(E) code_block(B). {

    S = malloc(sizeof(struct nettle_if_statement));
    S->expression = E;
    S->statements = B;
    S->next = NULL;
    printf("if\n");
}
if_statement(S) ::= IF expression(E) code_block(B) elseif_statement(N). {
    S = malloc(sizeof(struct nettle_if_statement));
    S->expression = E;
    S->statements = B;
    S->next = N;
    printf("if...\n");
}

elseif_statement(S) ::= ELSEIF expression(E) code_block(B) elseif_statement(N). { 
    S = malloc(sizeof(struct nettle_if_statement));
    S->expression = E;
    S->statements = B;
    S->next = N;
    printf("elseif ...\n"); 
}
elseif_statement(S) ::= ELSEIF expression(E) code_block(B). { 
    S = malloc(sizeof(struct nettle_if_statement));
    S->expression = E;
    S->statements = B;
    S->next = NULL;
    printf("elseif\n"); 
}

while_statement(S) ::= WHILE expression(E) code_block(B). {
    S = malloc(sizeof(struct nettle_while_statement));
    S->expression = E;
    S->statements = B;
    printf("while\n");
}

// /shrug
expression(D) ::= expression(A) PLUS(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) MINUS(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) MULTIPLY(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) DIVIDE(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) MODULO(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) EQUAL(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) INEQUAL(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) LSHIFT(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) RSHIFT(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) LAND(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) LOR(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) BAND(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) BOR(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) XOR(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }

expression(D) ::= expression(A) LTHAN(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) GTHAN(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) LETHAN(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= expression(A) GETHAN(O) expression(B). { D = malloc(sizeof(struct nettle_parser_expression)); D->children[0] = A; D->children[1] = B; D->token = O; }
expression(D) ::= NPARENL expression(E) NPARENR. { D = E; }
expression(D) ::= PLUS expression(E). [POSITIVE] { D = E; }
expression ::= MINUS expression. [NEGATIVE]
expression ::= NOT expression.

expression(EX) ::= INTEGER(VAL). { EX = malloc(sizeof(struct nettle_parser_expression)); EX->token = VAL; }
expression(EX) ::= STRING(VAL). { EX = malloc(sizeof(struct nettle_parser_expression)); EX->token = VAL; }
expression(EX) ::= IDENTIFIER(VAL). { EX = malloc(sizeof(struct nettle_parser_expression)); EX->token = VAL; }
// expression parsing

