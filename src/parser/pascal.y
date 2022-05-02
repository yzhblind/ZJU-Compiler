%locations
%define parse.error verbose

%code requires {

}

%{
extern int yylex();
extern void yyerror(const char*);
%}

%union{
    int64_t token_type;
    int64_t int_val;
    double real_val;
    bool bool_val;
    char char_val;
    char *text;
}

%token<token_type> KEY_AND
%token<token_type> KEY_ARRAY
%token<token_type> KEY_BEGIN
%token<token_type> KEY_CASE
%token<token_type> KEY_CONST
%token<token_type> KEY_DIV
%token<token_type> KEY_DO
%token<token_type> KEY_DOWNTO
%token<token_type> KEY_ELSE
%token<token_type> KEY_END
%token<token_type> KEY_FILE
%token<token_type> KEY_FOR
%token<token_type> KEY_FUNCTION
%token<token_type> KEY_GOTO
%token<token_type> KEY_IF
%token<token_type> KEY_IN
%token<token_type> KEY_LABEL
%token<token_type> KEY_MOD
%token<token_type> KEY_NIL
%token<token_type> KEY_NOT
%token<token_type> KEY_OF
%token<token_type> KEY_OR
%token<token_type> KEY_PACKED
%token<token_type> KEY_PROCEDURE
%token<token_type> KEY_PROGRAM
%token<token_type> KEY_RECORD
%token<token_type> KEY_REPEAT
%token<token_type> KEY_SET
%token<token_type> KEY_THEN
%token<token_type> KEY_TO
%token<token_type> KEY_TYPE
%token<token_type> KEY_UNTIL
%token<token_type> KEY_VAR
%token<token_type> KEY_WHILE
%token<token_type> KEY_WITH

%token<token_type> OP_ADD
%token<token_type> OP_MINUS
%token<token_type> OP_MUL
%token<token_type> OP_DIV
%token<token_type> OP_CMP
%token<token_type> OP_LT
%token<token_type> OP_GT
%token<token_type> OP_L_BCK
%token<token_type> OP_R_BCK
%token<token_type> OP_DOT
%token<token_type> OP_COMMA
%token<token_type> OP_COLON
%token<token_type> OP_SEMICOLON
%token<token_type> OP_CARET
%token<token_type> OP_L_PRTS
%token<token_type> OP_R_PRTS
%token<token_type> OP_DDOT


%token<text> IDENTIFIER

%%

%%