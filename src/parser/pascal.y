%locations
%define parse.error verbose

%code requires {
#include"ast/ast_all.hpp"
#include<cstdlib>
#include<cstdint>
extern ASTRoot *root_entry;
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
    ASTProcStmt *ast_proc_stmt;
    ASTReadPara *ast_read_para;
    ASTWritePara *ast_write_para;
    ASTActualPara *ast_actual_para;
    ASTVarAccess *ast_var_access;
    ASTAssignStmt *ast_assign_stmt;
    ASTIfStmt *ast_if_stmt;
    ASTRepeatStmt *ast_repeat_stmt;
    ASTWhileStmt *ast_while_stmt;
    ASTForStmt *ast_for_stmt;
    ASTExpr *ast_expr;
    ASTSimpleExpr *ast_simple_expr;
    ASTTerm *ast_term;
    ASTFactor *ast_factor;
    ASTVarAccessId *ast_var_id;
    ASTVarAccessIndex *ast_var_index;
    ASTVarAccessField *ast_var_field;
    ASTVarAccessPointer *ast_var_pointer;
    ASTFactorFunc *ast_factor_func;
    ASTExpr::ROP rop;
    ASTSimpleExpr::AOP aop;
    ASTTerm::MOP mop;
    WritePack *write_pack;
    vector<char *> *id_list;
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
%type<ast_stmt> STATEMENT
%type<ast_stmt> SIMPLE_STATEMENT
%type<ast_stmt> STRUCTURED_STATEMENT
%type<ast_stmt> CONDITIONAL_STATEMENT
%type<ast_stmt> REPETITIVE_STATEMENT
%type<ast_if_stmt> IF_STATEMENT
%type<ast_repeat_stmt> REPEAT_STATEMENT
%type<ast_while_stmt> WHILE_STATEMENT
%type<ast_proc_stmt> PROCEDURE_STATEMENT
%type<ast_proc_stmt> IO_PROCEDURE_STATEMENT
%type<ast_read_para> READ_PARA_LIST
%type<ast_read_para> READLN_PARA_LIST
%type<ast_read_para> VARIABLE_ACCESSES
%type<ast_write_para> WRITE_PARA_LIST
%type<ast_write_para> WRITELN_PARA_LIST
%type<ast_write_para> WRITE_PARAS
%type<write_pack> WRITE_PARA
%type<ast_actual_para> ACTUAL_PARA_LIST
%type<ast_actual_para> ACTUAL_PARAS
%type<ast_actual_para> ACTUAL_PARA
%type<ast_var_access> VARIABLE_ACCESS
%type<ast_assign_stmt> ASSIGNMENT_STATEMENT
%type<ast_for_stmt> FOR_STATEMENT
%type<ast_expr> BOOLEAN_EXPRESSION
%type<ast_expr> INITIAL_VARIABLE
%type<ast_expr> FINAL_VALUE
%type<ast_expr> EXPRESSION
%type<ast_expr> INDEX_EXPRESSION
%type<rop> RELATIONAL_OP
%type<ast_var_id> CONTROL_VARIABLE
%type<ast_var_id> ENTIRE_VARIABLE
%type<ast_var_access> COMPONENT_VARIABLE
%type<ast_var_access> POINTER_VARIABLE
%type<ast_var_access> ARRAY_VARIABLE
%type<ast_var_access> RECORD_VARIABLE
%type<ast_var_pointer> IDENTIFIED_VARIABLE
%type<ast_simple_expr> SIMPLE_EXPRESSION
%type<ast_var_field> FIELD_DESIGNATOR
%type<aop> ADDING_OP
%type<ast_term> TERM
%type<mop> MULTIPLYING_OP
%type<ast_var_index> INDEXED_VARIABLE
%type<ast_var_index> INDEX_EXPRESSIONS
%type<ast_factor> FACTOR
%type<ast_factor_func> FUNCTION_DESIGNATOR
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
%type<text> VARIABLE_IDENTIFIER
%type<text> FIELD_IDENTIFIER
%type<text> FUNCTION_IDENTIFIER
%type<text> FIELD_SPECIFIER

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
        $$ = new ASTRoot($1, $2, $3, nullptr, $4);
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
        $$ = new ASTVarDecl(*$1, $3);
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
        $$ = new ASTProcFuncBlock($1, $2, $3, $4);
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
        $$ = new ASTFuncDecl($2, nullptr, $4);
    }

FUNCTION_BLOCK:
    CONST_DEFS TYPE_DEFS VAR_DECLS STATEMENT_PART {
        $$ = new ASTProcFuncBlock($1, $2, $3, $4);
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
        $$ = new ASTVarDecl(*$1, new ASTTypeId($3));
    }

VARIABLE_PARA_SPEC:
    KEY_VAR IDENTIFIER_LIST OP_COLON TYPE_IDENTIFIER {
        $$ = new ASTVarDecl(*$2, new ASTTypeId($4));
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
        $2->set_packed_flag();
        $$ = $2;
    }

NEW_POINTER_TYPE:
    OP_CARET DOMAIN_TYPE {
        $$ = new ASTTypePointer(new ASTTypeId($2));
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
        $$ = new ASTVarDecl(*$1, $3);
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
    PROCEDURE_IDENTIFIER ACTUAL_PARA_LIST {
        $$ = new ASTProcStmt($1, $2);
    }
    | PROCEDURE_IDENTIFIER {
        $$ = new ASTProcStmt($1);
    }
    | IO_PROCEDURE_STATEMENT {
        $$ = $1;
    }

IO_PROCEDURE_STATEMENT:
    IO_READ READ_PARA_LIST {
        $$ = new ASTReadStmt(false, $2);
    }
    | IO_READLN READLN_PARA_LIST {
         $$ = new ASTReadStmt(true, $2);
    }
    | IO_WRITE WRITE_PARA_LIST {
        $$ = new ASTWriteStmt(false, $2);
    }
    | IO_WRITELN WRITELN_PARA_LIST {
        $$ = new ASTWriteStmt(true, $2);
    }

ACTUAL_PARA_LIST:
    OP_L_PRTS ACTUAL_PARAS OP_R_PRTS {
        $$ = $2;
    }

ACTUAL_PARAS:
    ACTUAL_PARAS OP_COMMA ACTUAL_PARA {
        $$ = $1->append($3);
    }
    | ACTUAL_PARA {
        $$ = $1;
    }

READ_PARA_LIST:
    OP_L_PRTS VARIABLE_ACCESSES OP_R_PRTS {
        $$ = $2;
    }

VARIABLE_ACCESSES:
    VARIABLE_ACCESSES OP_COMMA VARIABLE_ACCESS {
        $$ = $1;
        $$->push_back($3);
    }
    | VARIABLE_ACCESS {
        $$ = new ASTReadPara($1);
    }

READLN_PARA_LIST:
    {
        $$ = nullptr;
    }
    | OP_L_PRTS VARIABLE_ACCESSES OP_R_PRTS {
        $$ = $2;
    }

WRITE_PARA_LIST:
    WRITE_PARAS {
        $$ = $1;
    } 

WRITE_PARAS:
    WRITE_PARAS OP_COMMA WRITE_PARA {
        $$ = $1;
        $$->push_back($3);
    } 
    | WRITE_PARA {
        $$ = new ASTWritePara($1);
    } 

WRITELN_PARA_LIST:
    {
        $$ = nullptr;
    }
    | WRITE_PARAS {
        $$ = $1;
    }

ACTUAL_PARA:
    EXPRESSION {
        $$ = new ASTActualPara($1);
    }
    | KEY_PROCEDURE PROCEDURE_IDENTIFIER {
        $$ = new ASTActualPara($2, false);
    }
    | KEY_FUNCTION FUNCTION_IDENTIFIER {
        $$ = new ASTActualPara($2, true);
    }

WRITE_PARA:
    EXPRESSION {
        $$ = new WritePack($1);
    }
    | EXPRESSION OP_COLON EXPRESSION {
        $$ = new WritePack($1, $3);
    }
    | EXPRESSION OP_COLON EXPRESSION OP_COLON EXPRESSION {
        $$ = new WritePack($1, $3, $5);
    }

STATEMENT_PART:
    COMPOUND_STATEMENT {
        $$ = $1;
    }

COMPOUND_STATEMENT:
    KEY_BEGIN STATEMENT_SEQS KEY_END {
        $$ = $2;
    }

STATEMENT_SEQS:
    STATEMENT {
        $$ = $1;
    }
    | STATEMENT_SEQS OP_SEMICOLON STATEMENT {
        $$ = $1->append($3);
    }

STATEMENT:
    SIMPLE_STATEMENT {
        $$ = $1;
    }
    | STRUCTURED_STATEMENT {
        $$ = $1;
    }

SIMPLE_STATEMENT:
    EMPTY_STATEMENT {
        $$ = new ASTStmt();
    }
    | ASSIGNMENT_STATEMENT {
        $$ = $1;
    }
    | PROCEDURE_STATEMENT {
        $$ = $1;
    }

STRUCTURED_STATEMENT:
    COMPOUND_STATEMENT {
        $$ = $1;
    }
    | CONDITIONAL_STATEMENT {
        $$ = $1;
    }
    | REPETITIVE_STATEMENT {
        $$ = $1;
    }

EMPTY_STATEMENT:

ASSIGNMENT_STATEMENT:
    VARIABLE_ACCESS OP_ASSIGN EXPRESSION {
        $$ = new ASTAssignStmt($1, $3);
    }
    | KEY_RESULT OP_ASSIGN EXPRESSION {
        $$ = new ASTAssignStmt($3);
    }

CONDITIONAL_STATEMENT:
    IF_STATEMENT {
        $$ = $1;
    }

REPETITIVE_STATEMENT:
    REPEAT_STATEMENT {
        $$ = $1;
    }
    | WHILE_STATEMENT {
        $$ = $1;
    }
    | FOR_STATEMENT {
        $$ = $1;
    }

IF_STATEMENT:
    KEY_IF BOOLEAN_EXPRESSION KEY_THEN STATEMENT {
        $$ = new ASTIfStmt($2, $4, nullptr);
    }
    | KEY_IF  BOOLEAN_EXPRESSION KEY_THEN STATEMENT KEY_ELSE STATEMENT {
        $$ = new ASTIfStmt($2, $4, $6);
    }

REPEAT_STATEMENT:
    KEY_REPEAT STATEMENT_SEQS KEY_UNTIL BOOLEAN_EXPRESSION {
        $$ = new ASTRepeatStmt($2, $4);
    }

WHILE_STATEMENT:
    KEY_WHILE BOOLEAN_EXPRESSION KEY_DO STATEMENT {
        $$ = new ASTWhileStmt($2, $4);
    }

FOR_STATEMENT:
    KEY_FOR CONTROL_VARIABLE OP_ASSIGN INITIAL_VARIABLE KEY_TO FINAL_VALUE KEY_DO STATEMENT {
        $$ = new ASTForStmt($2, $4, $6, false, $8);
    }
    | KEY_FOR CONTROL_VARIABLE OP_ASSIGN INITIAL_VARIABLE KEY_DOWNTO FINAL_VALUE KEY_DO STATEMENT {
        $$ = new ASTForStmt($2, $4, $6, true, $8);
    }

BOOLEAN_EXPRESSION:
    EXPRESSION {
        $$ = $1;
    }

CONTROL_VARIABLE:
    ENTIRE_VARIABLE {
        $$ = $1;
    }

INITIAL_VARIABLE:
    EXPRESSION {
        $$ = $1;
    }

FINAL_VALUE:
    EXPRESSION {
        $$ = $1;
    }

    /* 表达式和变量 */

EXPRESSION:
    SIMPLE_EXPRESSION {
        $$ = new ASTExpr($1);
    }
    | SIMPLE_EXPRESSION RELATIONAL_OP SIMPLE_EXPRESSION {
        $$ = new ASTExpr($1, $2, $3);
    }
 
RELATIONAL_OP:
    OP_EQ {
        $$ = ASTExpr::EQ;
    }
    | OP_LT {
        $$ = ASTExpr::LT;
    }
    | OP_GT {
        $$ = ASTExpr::GT;
    }
    | OP_NOT_EQ {
        $$ = ASTExpr::NOT_EQ;
    }
    | OP_LE {
        $$ = ASTExpr::LE;
    }
    | OP_GE {
        $$ = ASTExpr::GE;
    }

VARIABLE_ACCESS:
    ENTIRE_VARIABLE {
        $$ = $1;
    }
    | COMPONENT_VARIABLE {
        $$ = $1;
    }
    | IDENTIFIED_VARIABLE {
        $$ = $1;
    }

FUNCTION_IDENTIFIER:
    IDENTIFIER {
        $$ = $1;
    }

ENTIRE_VARIABLE:
    VARIABLE_IDENTIFIER {
        $$ = new ASTVarAccessId($1);
    }

COMPONENT_VARIABLE:
    INDEXED_VARIABLE {
        $$ = $1;
    }
    | FIELD_DESIGNATOR {
        $$ = $1;
    }

IDENTIFIED_VARIABLE:
    POINTER_VARIABLE OP_CARET {
        $$ = new ASTVarAccessPointer($1);
    }

SIMPLE_EXPRESSION:
    TERM {
        $$ = new ASTSimpleExpr(false, $1);
    }
    | SIGN TERM {
        $$ = new ASTSimpleExpr($1==OP_SUB, $2);
    }
    | TERM ADDING_OP TERM {
        $$ = new ASTSimpleExpr(false, $1, $2, $3);
    }
    | SIGN TERM ADDING_OP TERM {
        $$ = new ASTSimpleExpr($1==OP_SUB, $2, $3, $4);
    }

ADDING_OP:
    KEY_OR {
        $$ = ASTSimpleExpr::OR;
    }
    | OP_ADD {
        $$ = ASTSimpleExpr::ADD;
    }
    | OP_SUB {
        $$ = ASTSimpleExpr::SUB;
    }

SIGN:
    OP_ADD {
        $$ = $1;
    }
    | OP_SUB {
        $$ = $1;
    }

VARIABLE_IDENTIFIER:
    IDENTIFIER {
        $$ = $1;
    }

INDEXED_VARIABLE:
    ARRAY_VARIABLE OP_L_BCK INDEX_EXPRESSIONS OP_R_BCK {
        $$ = $3;
        $$->set_array($1);
    }

INDEX_EXPRESSIONS:
    INDEX_EXPRESSION {
        $$ = new ASTVarAccessIndex($1);
    }
    | INDEX_EXPRESSIONS OP_COMMA INDEX_EXPRESSION {
        $$ = $1;
        $$->push_back($3);
    }

FIELD_DESIGNATOR:
    RECORD_VARIABLE OP_DOT FIELD_SPECIFIER {
        $$ = new ASTVarAccessField($1, $3);
    }

POINTER_VARIABLE:
    VARIABLE_ACCESS {
        $$ = $1;
    }

TERM:
    FACTOR {
        $$ = new ASTTerm($1);
    }
    | FACTOR MULTIPLYING_OP FACTOR {
        $$ = new ASTTerm($1, $2, $3);
    }

MULTIPLYING_OP:
    KEY_AND {
        $$ = ASTTerm::AND;
    }
    | KEY_DIV {
        $$ = ASTTerm::INT_DIV;
    }
    | KEY_MOD {
        $$ = ASTTerm::MOD;
    }
    | OP_MUL {
        $$ = ASTTerm::MUL;
    }
    | OP_DIV {
        $$ = ASTTerm::FLT_DIV;
    }

ARRAY_VARIABLE:
    VARIABLE_ACCESS {
        $$ = $1;
    }

INDEX_EXPRESSION:
    EXPRESSION {
        $$ = $1;
    }

RECORD_VARIABLE:
    VARIABLE_ACCESS {
        $$ = $1;
    }

FIELD_SPECIFIER:
    FIELD_IDENTIFIER {
        $$ = $1;
    }

    /* 注意处理值和引用 */

FACTOR:
    VARIABLE_ACCESS {
        $$ = new ASTFactorVar($1);
    }
    | UNSIGNED_CONSTANT {
        $$ = new ASTFactorConst($1);
    }
    | FUNCTION_DESIGNATOR {
        $$ = $1;
    }
    | OP_L_PRTS EXPRESSION OP_R_PRTS {
        $$ = new ASTFactorExpr($2);
    }
    | KEY_NOT FACTOR {
        $$ = $2;
        $$->flip_not();
    }
    | OP_AT VARIABLE_ACCESS {
        $$ = new ASTFactorAt($2);
    }

FIELD_IDENTIFIER:
    IDENTIFIER {
        $$ = $1;
    }

FUNCTION_DESIGNATOR:
    FUNCTION_IDENTIFIER OP_L_PRTS OP_R_PRTS {
        $$ = new ASTFactorFunc($1);
    }
    | FUNCTION_IDENTIFIER ACTUAL_PARA_LIST {
        $$ = new ASTFactorFunc($1, $2);
    }


%%