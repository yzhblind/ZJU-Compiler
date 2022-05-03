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
%token<token_type> KEY_FORWARD
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
%token<token_type> OP_EQ
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

PROGRAM:
    PROGRAM_HEADING OP_SEMICOLON PROGRAM_BLOCK


PROGRAM_HEADING:
    PROGRAM IDENTIFIER OP_L_PRTS PROGRAM_PARAMETERS OP_R_PRTS


PROGRAM_PARAMETERS: |
    IDENTIFIER_LIST

PROGRAM_BLOCK:
    BLOCK OP_DOT

BLOCK:
    CONST_DEFS TYPE_DEFS VAR_DECLS PROCEDURE_FUNCTION_DECLS STATEMENT_PART

CONST_DEFS: |
    KEY_CONST CONST_DEF_SEQ OP_SEMICOLON

CONST_DEF_SEQ:
    CONST_DEF_SEQ OP_SEMICOLON CONST_DEF |
    CONST_DEF

CONST_DEF:
    IDENTIFIER OP_EQ CONSTANT

TYPE_DEFS: |
    KEY_TYPE TYPE_DEF_SEQ OP_SEMICOLON

TYPE_DEF_SEQ:
    TYPE_DEF_SEQ OP_SEMICOLON TYPE_DEF |
    TYPE_DEF

TYPE_DEF:
    IDENTIFIER OP_EQ TYPE_DENOTER

VAR_DECLS: |
    KEY_VAR VAR_DECL_SEQ OP_SEMICOLON

VAR_DECL_SEQ:
    VAR_DECL_SEQ OP_SEMICOLON VAR_DECL |
    VAR_DECL

VAR_DECL:
    IDENTIFIER_LIST OP_COLON TYPE_DENOTER

PROCEDURE_FUNCTION_DECLS: |
    PROCEDURE_FUNCTION_DECLS SUBPROGRAM_DECL OP_SEMICOLON |
    SUBPROGRAM_DECL OP_SEMICOLON

SUBPROGRAM_DECL:
    PROCEDURE_DECL | FUNCTION_DECL

IDENTIFIER_LIST:
    IDENTIFIER_LIST OP_COMMA IDENTIFIER |
    IDENTIFIER

PROCEDURE_DECL:
    PROCEDURE_HEADING OP_SEMICOLON DIRECTIVE |
    PROCEDURE_HEADING OP_SEMICOLON PROCEDURE_BLOCK

PROCEDURE_HEADING:
    KEY_PROCEDURE IDENTIFIER |
    KEY_PROCEDURE IDENTIFIER FORMAL_PARAMETER_LIST

PROCEDURE_BLOCK:
    CONST_DEFS TYPE_DEFS VAR_DECLS STATEMENT_PART

DIRECTIVE:
    KEY_FORWARD

TYPE_DENOTER:
    TYPE_IDENTIFIER |
    NEW_TYPE

FUNCTION_DECL:
    FUNCTION_HEADING OP_SEMICOLON DIRECTIVE |
    FUNCTION_HEADING OP_SEMICOLON FUNCTION_BLOCK

FUNCTION_HEADING:
    KEY_FUNCTION IDENTIFIER FORMAL_PARAMETER_LIST OP_COLON RESULT_TYPE |
    KEY_FUNCTION IDENTIFIER OP_COLON RESULT_TYPE

FUNCTION_BLOCK:
    CONST_DEFS TYPE_DEFS VAR_DECLS STATEMENT_PART

FORMAL_PARAMETER_LIST:
    OP_L_PRTS FORMAL_PARAMETER_SECTIONS OP_R_PRTS

FORMAL_PARAMETER_SECTIONS:
    FORMAL_PARAMETER_SECTIONS OP_SEMICOLON FORMAL_PARAMETER_SECTION |
    FORMAL_PARAMETER_SECTION

FORMAL_PARAMETER_SECTION:
    VALUE_PARA_SPEC |
    VARIABLE_PARA_SPEC |
    PROCEDURE_PARA_SEPC |
    FUNCTION_PARA_SEPC |
    CONFORMANT_ARRAY_PARA_SEPC

// TODO

%%