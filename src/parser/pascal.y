%locations
%define parse.error verbose

%code requires {
#include<cstdlib>
#include<cstdint>
}

%{
extern ASTRoot *root_entry;
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
    ASTRoot* ast_root;
    ASTConstDef *ast_const_def;
    ASTTypeDef *ast_type_def;
    ASTVarDecl *ast_var_decl;
    ASTParameter *ast_para;
    ASTProcFuncDecl *ast_proc_func_decl;
    ASTProcDecl *ast_proc_decl;
    ASTFuncDecl *ast_func_decl;
    ASTProcFuncBlock *ast_proc_func_block;
    ASTStmt *ast_stmt;
    ASTConstValue *ast_const_value;
    ASTType *ast_type;
    ASTTypeId *ast_type_id;
    ASTTypeSubrange *ast_type_subrange;
    ASTTypeStructure *ast_type_structure;
    ASTTypeArray *ast_type_array;
    ASTTypeRecord *ast_type_record;
    ASTTypePointer *ast_type_pointer;
    vector<char *> id_list;
}

%type<ast_root> PROGRAM_BLOCK
%type<ast_root> BLOCK
%type<ast_const_def> CONST_DEFS
%type<ast_const_def> CONST_DEF_SEQ
%type<ast_const_def> CONST_DEF
%type<ast_type_def> TYPE_DEFS
%type<ast_type_def> TYPE_DEF_SEQ
%type<ast_type_def> TYPE_DEF
%type<ast_type> TYPE_DENOTER
%type<ast_type> NEW_TYPE
%type<ast_type> COMPONENT_TYPE
%type<ast_type_subrange> NEW_ORDINAL_TYPE
%type<ast_type_structure> NEW_STRUCTURED_TYPE
%type<ast_type_structure> UNPACKED_STRUCTURE_TYPE
%type<ast_type_array> ARRAY_TYPE
%type<ast_type_array> INDEX_TYPES
%type<ast_type_record> RECORD_TYPE
%type<ast_type_record> FIXED_PART
%type<ast_type_record> FIELD_LIST
%type<ast_type_pointer> NEW_POINTER_TYPE
%type<ast_type_subrange> SUBRANGE_TYPE
%type<ast_type_subrange> ORDINAL_TYPE
%type<ast_type_subrange> INDEX_TYPE
%type<ast_var_decl> VAR_DECLS
%type<ast_var_decl> VAR_DECL_SEQ
%type<ast_var_decl> VAR_DECL
%type<ast_var_decl> RECORD_SECTION
%type<ast_var_decl> VALUE_PARA_SPEC
%type<ast_var_decl> VARIABLE_PARA_SPEC
%type<ast_proc_func_decl> PROCEDURE_FUNCTION_DECLS
%type<ast_proc_func_decl> SUBPROGRAM_DECL
%type<ast_proc_decl> PROCEDURE_DECL
%type<ast_proc_decl> PROCEDURE_HEADING
%type<ast_proc_decl> PROCEDURE_PARA_SEPC
%type<ast_func_decl> FUNCTION_DECL
%type<ast_func_decl> FUNCTION_HEADING
%type<ast_func_decl> FUNCTION_PARA_SEPC
%type<ast_para> FORMAL_PARAMETER_LIST
%type<ast_para> FORMAL_PARAMETER_SECTIONS
%type<ast_para> FORMAL_PARAMETER_SECTION
%type<ast_proc_func_block> PROCEDURE_BLOCK
%type<ast_proc_func_block> FUNCTION_BLOCK
%type<ast_stmt> STATEMENT_PART
%type<ast_stmt> COMPOUND_STATEMENT
%type<ast_stmt> STATEMENT_SEQS
%type<ast_const_value> CONSTANT
%type<ast_const_value> UNSIGNED_CONSTANT
%type<ast_const_value> SIGNED_NUMBER
%type<ast_const_value> UNSIGNED_NUMBER
%type<real_val> SIGNED_REAL
%type<int_val> SIGNED_INTEGER
%type<token_type> SIGN
%type<id_list> IDENTIFIER_LIST
%type<text> CONSTANT_IDENTIFIER
%type<text> TYPE_IDENTIFIER
%type<text> RESULT_TYPE
%type<text> DOMAIN_TYPE
%type<text> ORDINAL_TYPE_IDENTIFIER
%type<text> PROCEDURE_IDENTIFIER

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
%token<token_type> KEY_FOR
%token<token_type> KEY_FORWARD
%token<token_type> KEY_FUNCTION
%token<token_type> KEY_IF
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
%token<token_type> KEY_RESULT
%token<token_type> KEY_THEN
%token<token_type> KEY_TO
%token<token_type> KEY_TYPE
%token<token_type> KEY_UNTIL
%token<token_type> KEY_VAR
%token<token_type> KEY_WHILE
%token<token_type> KEY_WITH

%token<token_type> OP_ADD
%token<token_type> OP_SUB
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
%token<token_type> OP_NOT_EQ
%token<token_type> OP_LE 
%token<token_type> OP_GE
%token<token_type> OP_ASSIGN
%token<token_type> OP_DDOT
%token<token_type> OP_AT

%token<token_type> IO_READ
%token<token_type> IO_WRITE
%token<token_type> IO_READLN
%token<token_type> IO_WRITELN

%token<text> IDENTIFIER

%token<text> CHARACTER_STRING
%token<int_val> UNSIGNED_INTEGER
%token<real_val> UNSIGNED_REAL 

%left KEY_OR OP_ADD OP_SUB
%left KEY_AND KEY_DIV KEY_MOD OP_MUL OP_DIV
%left OP_EQ OP_LT OP_GT OP_NOT_EQ OP_LE OP_GE

%precedence KEY_THEN
%precedence KEY_ELSE

%%
    /* 程序和块 */
PROGRAM:
    PROGRAM_HEADING OP_SEMICOLON PROGRAM_BLOCK {
        root_entry = $3;
    }

PROGRAM_HEADING:
    KEY_PROGRAM IDENTIFIER OP_L_PRTS PROGRAM_PARAMETERS OP_R_PRTS

PROGRAM_PARAMETERS:
    | IDENTIFIER_LIST

PROGRAM_BLOCK:
    BLOCK OP_DOT {
        $$ = $1;
    }

BLOCK:
    CONST_DEFS TYPE_DEFS VAR_DECLS PROCEDURE_FUNCTION_DECLS STATEMENT_PART {
        $$ = new ASTRoot($1, $2, $3, $4, $5);
    }
    | CONST_DEFS TYPE_DEFS VAR_DECLS STATEMENT_PART {
        $$ = new ASTRoot($1, $2, $3, nullptr, $5);
    }

    /* 声明和类型定义 */

CONST_DEFS: 
    {
        $$ = nullptr;
    }
    | KEY_CONST CONST_DEF_SEQ OP_SEMICOLON {
        $$ = $2;
    }

CONST_DEF_SEQ:
    CONST_DEF_SEQ OP_SEMICOLON CONST_DEF {
        $$ = $1->append($3);
    }
    | CONST_DEF {
        $$ = $1;
    }

CONST_DEF:
    IDENTIFIER OP_EQ CONSTANT {
        $$ = new ASTConstDef($1, $3);
    }

TYPE_DEFS:
    {
        $$ = nullptr;
    }
    | KEY_TYPE TYPE_DEF_SEQ OP_SEMICOLON {
        $$ = $2;
    }

TYPE_DEF_SEQ:
    TYPE_DEF_SEQ OP_SEMICOLON TYPE_DEF {
        $$ = $1->append($3);
    }
    | TYPE_DEF {
        $$ = $1;
    }

TYPE_DEF:
    IDENTIFIER OP_EQ TYPE_DENOTER {
        $$ = new ASTTypeDef($1, $3);
    }

VAR_DECLS:
    {
        $$ = nullptr;
    }
    | KEY_VAR VAR_DECL_SEQ OP_SEMICOLON {
        $$ = $2;
    }

VAR_DECL_SEQ:
    VAR_DECL_SEQ OP_SEMICOLON VAR_DECL {
        $$ = $1->append($3);
    }
    | VAR_DECL {
        $$ = $1;
    }

VAR_DECL:
    IDENTIFIER_LIST OP_COLON TYPE_DENOTER {
        $$ = new ASTVarDecl($1, $3);
    }

PROCEDURE_FUNCTION_DECLS:
    PROCEDURE_FUNCTION_DECLS SUBPROGRAM_DECL OP_SEMICOLON {
        $$ = $1->append($2);
    }
    | SUBPROGRAM_DECL OP_SEMICOLON {
        $$ = $1;
    }

IDENTIFIER_LIST:
    IDENTIFIER_LIST OP_COMMA IDENTIFIER {
        $$ = $1;
        $$->push_back($3);
    }
    | IDENTIFIER {
        $$ = new vector<char *>;
        $$->push_back($1);
    }

    /* 子程序声明 */

SUBPROGRAM_DECL:
    PROCEDURE_DECL {
        $$ = $1;
    }
    | FUNCTION_DECL {
        $$ = $1;
    }

    /* 过程声明 */

PROCEDURE_DECL:
    PROCEDURE_HEADING OP_SEMICOLON DIRECTIVE {
        $$ = $1;
        $$->set_forward();
    }
    | PROCEDURE_HEADING OP_SEMICOLON PROCEDURE_BLOCK {
        $$ = $1;
        $$->set_block($3);
    }

PROCEDURE_HEADING:
    KEY_PROCEDURE IDENTIFIER {
        $$ = new ASTProcDecl($2, nullptr);
    }
    | KEY_PROCEDURE IDENTIFIER FORMAL_PARAMETER_LIST {
        $$ = new ASTProcDecl($2, $3);
    }

PROCEDURE_BLOCK:
    CONST_DEFS TYPE_DEFS VAR_DECLS STATEMENT_PART {
        $$ = new ASTProcFuncBlock($1, $2, $3, $3);
    }

PROCEDURE_IDENTIFIER:
    IDENTIFIER {
        $$ = $1;
    }

DIRECTIVE:
    KEY_FORWARD

TYPE_DENOTER:
    TYPE_IDENTIFIER {
        $$ = new ASTTypeId($1);
    }
    | NEW_TYPE {
        $$ = $1;
    }

    /* 函数声明 */

FUNCTION_DECL:
    FUNCTION_HEADING OP_SEMICOLON DIRECTIVE {
        $$ = $1;
        $$->set_forward();
    }
    | FUNCTION_HEADING OP_SEMICOLON FUNCTION_BLOCK {
        $$ = $1;
        $$->set_block($3);
    }

FUNCTION_HEADING:
    KEY_FUNCTION IDENTIFIER FORMAL_PARAMETER_LIST OP_COLON RESULT_TYPE {
        $$ = new ASTFuncDecl($2, $3, $5);
    }
    | KEY_FUNCTION IDENTIFIER OP_COLON RESULT_TYPE {
        $$ = new ASTFuncDecl($2, nullptr, $5);
    }

FUNCTION_BLOCK:
    CONST_DEFS TYPE_DEFS VAR_DECLS STATEMENT_PART {
        $$ = new ASTProcFuncBlock($1, $2, $3, $3);
    }

FORMAL_PARAMETER_LIST:
    OP_L_PRTS FORMAL_PARAMETER_SECTIONS OP_R_PRTS {
        $$ = $2;
    }

FORMAL_PARAMETER_SECTIONS:
    FORMAL_PARAMETER_SECTIONS OP_SEMICOLON FORMAL_PARAMETER_SECTION {
        $$ = $1->append($3);
    }
    | FORMAL_PARAMETER_SECTION {
        $$ = $1;
    }

FORMAL_PARAMETER_SECTION:
    VALUE_PARA_SPEC {
        $$ = new ASTParameter($1, false);
    }
    | VARIABLE_PARA_SPEC {
        $$ = new ASTParameter($1, true);
    }
    | PROCEDURE_PARA_SEPC {
        $$ = new ASTParameter($1);
    }
    | FUNCTION_PARA_SEPC {
        $$ = new ASTParameter($1);
    }
    | CONFORMANT_ARRAY_PARA_SEPC {
        // TODO
        $$ = nullptr;
    }

VALUE_PARA_SPEC:
    IDENTIFIER_LIST OP_COLON TYPE_IDENTIFIER {
        $$ = new ASTVarDecl($1, new ASTTypeId($3));
    }

VARIABLE_PARA_SPEC:
    KEY_VAR IDENTIFIER_LIST OP_COLON TYPE_IDENTIFIER {
        $$ = new ASTVarDecl($2, new ASTTypeId($4));
    }

PROCEDURE_PARA_SEPC:
    PROCEDURE_HEADING {
        $$ = $1;
    }

FUNCTION_PARA_SEPC:
    FUNCTION_HEADING {
        $$ = $1;
    }

CONFORMANT_ARRAY_PARA_SEPC:
    VALUE_CONFORMANT_ARRAY_SEPC
    | VARIABLE_CONFORMANT_ARRAY_SEPC

VALUE_CONFORMANT_ARRAY_SEPC:
    IDENTIFIER_LIST OP_COLON CONFORMANT_ARRAY_SCHEMA

VARIABLE_CONFORMANT_ARRAY_SEPC:
    KEY_VAR IDENTIFIER_LIST OP_COLON CONFORMANT_ARRAY_SCHEMA

CONFORMANT_ARRAY_SCHEMA:
    PACKED_CONFORMANT_ARRAY_SCHEMA
    | UNPACKED_CONFORMANT_ARRAY_SCHEMA

PACKED_CONFORMANT_ARRAY_SCHEMA:
    KEY_PACKED KEY_ARRAY OP_L_BCK INDEX_TYPE_SPEC OP_R_BCK KEY_OF TYPE_IDENTIFIER

UNPACKED_CONFORMANT_ARRAY_SCHEMA:
    KEY_ARRAY OP_L_BCK INDEX_TYPE_SPECS OP_R_BCK KEY_OF TYPE_IDENTIFIER
    | KEY_ARRAY OP_L_BCK INDEX_TYPE_SPECS OP_R_BCK KEY_OF CONFORMANT_ARRAY_SCHEMA

INDEX_TYPE_SPECS:
    INDEX_TYPE_SPECS OP_SEMICOLON INDEX_TYPE_SPEC
    | INDEX_TYPE_SPEC

INDEX_TYPE_SPEC:
    IDENTIFIER OP_DDOT IDENTIFIER OP_COLON ORDINAL_TYPE_IDENTIFIER

    /* 常量 */

CONSTANT:
    SIGNED_NUMBER {
        $$ = $1;
    }
    | CONSTANT_IDENTIFIER {
        $$ = new ASTConstValue($1, true);
    }
    | CHARACTER_STRING {
        $$ = new ASTConstValue($1, false);
    }

UNSIGNED_CONSTANT:
    UNSIGNED_NUMBER {
        $$ = $1;
    }
    | CHARACTER_STRING {
        $$ = new ASTConstValue($1, false);
    }
    | KEY_NIL {
        $$ = new ASTConstValue();
    }

SIGNED_NUMBER:
    SIGNED_INTEGER {
        $$ = new ASTConstValue($1);
    }
    | SIGNED_REAL {
        $$ = new ASTConstValue($1);
    }

UNSIGNED_NUMBER:
    UNSIGNED_INTEGER {
        $$ = new ASTConstValue($1);
    }
    | UNSIGNED_REAL {
        $$ = new ASTConstValue($1);
    }

SIGNED_INTEGER:
    SIGN UNSIGNED_INTEGER{
        if($1 == OP_SUB)
            $$ = -$2;
        else
            $$ = $2;
    }
    | UNSIGNED_INTEGER {
        $$ = $1;
    }

SIGNED_REAL:
    SIGN UNSIGNED_REAL {
        if($1 == OP_SUB)
            $$ = -$2;
        else
            $$ = $2;
    }
    | UNSIGNED_REAL {
        $$ = $1;
    }

CONSTANT_IDENTIFIER:
    IDENTIFIER {
        $$ = $1;
    }

    /* 类型 */ 

TYPE_IDENTIFIER:
    IDENTIFIER {
        $$ = $1;
    }

NEW_TYPE:
    NEW_ORDINAL_TYPE {
        $$ = $1;
    }
    | NEW_STRUCTURED_TYPE {
        $$ = $1;
    }
    | NEW_POINTER_TYPE {
        $$ = $1;
    }

RESULT_TYPE:
    TYPE_IDENTIFIER {
        $$ = $1;
    }

NEW_ORDINAL_TYPE:
    SUBRANGE_TYPE {
        $$ = $1;
    }
    
NEW_STRUCTURED_TYPE:
    UNPACKED_STRUCTURE_TYPE {
        $$ = $1;
    }
    | KEY_PACKED UNPACKED_STRUCTURE_TYPE {
        $1->set_packed_flag();
        $$ = $1;
    }

NEW_POINTER_TYPE:
    OP_CARET DOMAIN_TYPE {
        $$ = new ASTTypePointer($2);
    }

SUBRANGE_TYPE:
    CONSTANT OP_DDOT CONSTANT {
        $$ = new ASTTypeSubrange($1, $3);
    }

UNPACKED_STRUCTURE_TYPE:
    ARRAY_TYPE {
        $$ = $1;
    }
    | RECORD_TYPE {
        $$ = $1;
    }

DOMAIN_TYPE:
    TYPE_IDENTIFIER {
        $$ = $1;
    }

ARRAY_TYPE:
    KEY_ARRAY OP_L_BCK INDEX_TYPES OP_R_BCK KEY_OF COMPONENT_TYPE {
        $$ = $3;
        $$->set_element_type($6);
    }

INDEX_TYPES:
    INDEX_TYPES OP_COMMA INDEX_TYPE {
        $$ = $1;
        $$->push_index($3);
    }
    | INDEX_TYPE {
        $$ = new ASTTypeArray();
        $$->push_index($1);
    }

INDEX_TYPE:
    ORDINAL_TYPE {
        $$ = $1;
    }

COMPONENT_TYPE:
    TYPE_DENOTER {
        $$ = $1;
    }

ORDINAL_TYPE:
    NEW_ORDINAL_TYPE {
        $$ = $1;
    }
    | ORDINAL_TYPE_IDENTIFIER {
        $$ = new ASTTypeSubrange($1);
    }

ORDINAL_TYPE_IDENTIFIER:
    TYPE_IDENTIFIER {
        $$ = $1;
    }

RECORD_TYPE:
    KEY_RECORD FIELD_LIST KEY_END {
        $$ = $2;
    }

RECORD_SECTION:
    IDENTIFIER_LIST OP_COLON TYPE_DENOTER {
        $$ = new ASTVarDecl($1, $3);
    }

FIELD_LIST:
    FIXED_PART OP_SEMICOLON {
        $$ = $1;
    }

FIXED_PART:
    FIXED_PART OP_SEMICOLON RECORD_SECTION {
        $$ = $1;
        $$->push_var_decl($3);
    }
    | RECORD_SECTION {
        $$ = new ASTTypeRecord();
        $$->push_var_decl($1);
    }

    /* 语句 */

PROCEDURE_STATEMENT:
    PROCEDURE_IDENTIFIER ACTUAL_PARA_LIST
    | PROCEDURE_IDENTIFIER
    | IO_PROCEDURE_STATEMENT

IO_PROCEDURE_STATEMENT:
    IO_READ READ_PARA_LIST
    | IO_READLN READLN_PARA_LIST
    | IO_WRITE WRITE_PARA_LIST
    | IO_WRITELN WRITELN_PARA_LIST

ACTUAL_PARA_LIST:
    OP_L_PRTS ACTUAL_PARAS OP_R_PRTS

ACTUAL_PARAS:
    ACTUAL_PARAS OP_COMMA ACTUAL_PARA 
    | ACTUAL_PARA 

READ_PARA_LIST:
    OP_L_PRTS VARIABLE_ACCESSES OP_R_PRTS

VARIABLE_ACCESSES:
    VARIABLE_ACCESSES OP_COMMA VARIABLE_ACCESS
    | VARIABLE_ACCESS

READLN_PARA_LIST:
    | OP_L_PRTS VARIABLE_ACCESSES OP_R_PRTS

WRITE_PARA_LIST:
    WRITE_PARAS 

WRITE_PARAS:
    WRITE_PARAS OP_COMMA WRITE_PARA 
    | WRITE_PARA 

WRITELN_PARA_LIST:
    | WRITE_PARAS

ACTUAL_PARA:
    EXPRESSION
    | KEY_PROCEDURE PROCEDURE_IDENTIFIER
    | KEY_FUNCTION FUNCTION_IDENTIFIER

VARIABLE_ACCESS:
    ENTIRE_VARIABLE
    | COMPONENT_VARIABLE
    | IDENTIFIED_VARIABLE

WRITE_PARA:
    EXPRESSION
    | EXPRESSION OP_COLON EXPRESSION
    | EXPRESSION OP_COLON EXPRESSION OP_COLON EXPRESSION

STATEMENT_PART:
    COMPOUND_STATEMENT {
        $$ = $1;
    }

COMPOUND_STATEMENT:
    KEY_BEGIN STATEMENT_SEQS KEY_END {
        $$ = $2;
    }

STATEMENT_SEQS:
    STATEMENT
    | STATEMENT_SEQS OP_SEMICOLON STATEMENT

STATEMENT:
    SIMPLE_STATEMENT
    | STRUCTURED_STATEMENT

SIMPLE_STATEMENT:
    EMPTY_STATEMENT
    | ASSIGNMENT_STATEMENT
    | PROCEDURE_STATEMENT

STRUCTURED_STATEMENT:
    COMPOUND_STATEMENT
    | CONDITIONAL_STATEMENT
    | REPETITIVE_STATEMENT
    | WITH_STATEMENT

EMPTY_STATEMENT:

ASSIGNMENT_STATEMENT:
    VARIABLE_ACCESS OP_ASSIGN EXPRESSION
    | KEY_RESULT OP_ASSIGN EXPRESSION

CONDITIONAL_STATEMENT:
    IF_STATEMENT
    | CASE_STATEMENT

REPETITIVE_STATEMENT:
    REPEAT_STATEMENT
    | WHILE_STATEMENT
    | FOR_STATEMENT

WITH_STATEMENT:
    KEY_WITH RECORD_VARIABLE_LIST KEY_DO STATEMENT

IF_STATEMENT:
    KEY_IF  BOOLEAN_EXPRESSION KEY_THEN STATEMENT
    | KEY_IF  BOOLEAN_EXPRESSION KEY_THEN STATEMENT KEY_ELSE STATEMENT

CASE_STATEMENT:
    KEY_CASE CASE_INDEX KEY_OF CASE_LIST_ELEMENTS KEY_END
    | KEY_CASE CASE_INDEX KEY_OF CASE_LIST_ELEMENTS OP_SEMICOLON KEY_END

CASE_LIST_ELEMENTS:
    CASE_LIST_ELEMENT
    | CASE_LIST_ELEMENTS OP_SEMICOLON CASE_LIST_ELEMENT

REPEAT_STATEMENT:
    KEY_REPEAT STATEMENT_SEQS KEY_UNTIL BOOLEAN_EXPRESSION

WHILE_STATEMENT:
    KEY_WHILE BOOLEAN_EXPRESSION KEY_DO STATEMENT

FOR_STATEMENT:
    KEY_FOR CONTROL_VARIABLE OP_ASSIGN INITIAL_VARIABLE KEY_TO FINAL_VALUE KEY_DO STATEMENT
    | KEY_FOR CONTROL_VARIABLE OP_ASSIGN INITIAL_VARIABLE KEY_DOWNTO FINAL_VALUE KEY_DO STATEMENT

RECORD_VARIABLE_LIST:
    RECORD_VARIABLE
    | RECORD_VARIABLE_LIST OP_COMMA RECORD_VARIABLE

BOOLEAN_EXPRESSION:
    EXPRESSION

CASE_INDEX:
    EXPRESSION

CASE_LIST_ELEMENT:
    CASE_CONSTANT_LIST OP_COLON STATEMENT

CASE_CONSTANT_LIST:
    CASE_CONSTANT
    | CASE_CONSTANT_LIST OP_COMMA CASE_CONSTANT

CASE_CONSTANT:
    CONSTANT

CONTROL_VARIABLE:
    ENTIRE_VARIABLE

INITIAL_VARIABLE:
    EXPRESSION

FINAL_VALUE:
    EXPRESSION

    /* 表达式和变量 */

EXPRESSION:
    SIMPLE_EXPRESSION
    | SIMPLE_EXPRESSION RELATIONAL_OP SIMPLE_EXPRESSION
 
RELATIONAL_OP:
    OP_EQ
    | OP_LT
    | OP_GT
    | OP_NOT_EQ
    | OP_LE
    | OP_GE

FUNCTION_IDENTIFIER:
    IDENTIFIER

ENTIRE_VARIABLE:
    VARIABLE_IDENTIFIER

COMPONENT_VARIABLE:
    INDEXED_VARIABLE
    | FIELD_DESIGNATOR

IDENTIFIED_VARIABLE:
    POINTER_VARIABLE OP_CARET

SIMPLE_EXPRESSION:
    TERM
    | SIGN TERM
    | TERM ADDING_OP TERM
    | SIGN TERM ADDING_OP TERM

ADDING_OP:
    KEY_OR
    | OP_ADD
    | OP_SUB

SIGN:
    OP_ADD {
        $$ = $1;
    }
    | OP_SUB {
        $$ = $1;
    }

VARIABLE_IDENTIFIER:
    IDENTIFIER

INDEXED_VARIABLE:
    ARRAY_VARIABLE OP_L_BCK INDEX_EXPRESSIONS OP_R_BCK

INDEX_EXPRESSIONS:
    INDEX_EXPRESSION
    | INDEX_EXPRESSIONS OP_COMMA INDEX_EXPRESSION

FIELD_DESIGNATOR:
    RECORD_VARIABLE OP_DOT FIELD_SPECIFIER

POINTER_VARIABLE:
    VARIABLE_ACCESS

TERM:
    FACTOR
    | FACTOR MULTIPLYING_OP FACTOR

MULTIPLYING_OP:
    KEY_AND
    | KEY_DIV
    | KEY_MOD
    | OP_MUL
    | OP_DIV

ARRAY_VARIABLE:
    VARIABLE_ACCESS

INDEX_EXPRESSION:
    EXPRESSION

RECORD_VARIABLE:
    VARIABLE_ACCESS

FIELD_SPECIFIER:
    FIELD_IDENTIFIER

    /* 注意处理值和引用 */

FACTOR:
    VARIABLE_ACCESS
    | UNSIGNED_CONSTANT
    | FUNCTION_DESIGNATOR
    | OP_L_PRTS EXPRESSION OP_R_PRTS
    | KEY_NOT FACTOR
    | OP_AT VARIABLE_ACCESS

FIELD_IDENTIFIER:
    IDENTIFIER

FUNCTION_DESIGNATOR:
    FUNCTION_IDENTIFIER OP_L_PRTS OP_R_PRTS
    | FUNCTION_IDENTIFIER ACTUAL_PARA_LIST


%%

/* int main()
{
    yyparse();
    return 0;
} */