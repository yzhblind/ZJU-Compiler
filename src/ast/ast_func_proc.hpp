#pragma once

#include "ast_all.hpp"

class ASTParameter : public ASTNode
{
public:
    ASTParameter(ASTVarDecl *var_decl, bool is_ref);
    ASTParameter(ASTProcDecl *proc_decl);
    ASTParameter(ASTFuncDecl *func_decl);
    ~ASTParameter();
    ASTParameter *append(ASTParameter *next);
    ASTParameter *next_para;

    enum TypeKind{
        VARIABLE,
        REF_VARIABLE,
        PROCEDURE,
        FUNCTION
    };
    ASTParameter::TypeKind get_type();
    TypeKind type;

    ASTVarDecl *var_decl;
    ASTProcFuncDecl *proc_func;
};

class ASTProcFuncBlock : public ASTNode
{
public:
    ASTProcFuncBlock(ASTConstDef *const_def, ASTTypeDef *type_def, ASTVarDecl *var_decl, ASTStmt *stmt);
    ~ASTProcFuncBlock();
    ASTConstDef *const_def;
    ASTTypeDef *type_def;
    ASTVarDecl *var_decl;
    ASTStmt *stmt;
};

class ASTProcFuncDecl : public ASTNode
{
public:
    ~ASTProcFuncDecl();
    enum TypeKind
    {
        PROCEDURE,
        FUNCTION
    };
    virtual ASTProcFuncDecl::TypeKind get_type() = 0;
    ASTProcFuncDecl *append(ASTProcFuncDecl *next);
    void set_forward();
    void set_block(ASTProcFuncBlock *block);
    ASTProcFuncDecl *next_proc_func_decl;

    string id;
    ASTParameter *para;
    bool is_forward;
    ASTProcFuncBlock *block;
};

class ASTProcDecl : public ASTProcFuncDecl
{
public:
    ASTProcDecl(char *id, ASTParameter *para);
    ~ASTProcDecl();
    ASTProcFuncDecl::TypeKind get_type();
};

class ASTFuncDecl : public ASTProcFuncDecl
{
public:
    ASTFuncDecl(char *id, ASTParameter *para, char *result_id);
    ~ASTFuncDecl();
    ASTProcFuncDecl::TypeKind get_type();
    string ret_type_id;
};