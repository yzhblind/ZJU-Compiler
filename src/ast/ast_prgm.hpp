#pragma once

#include "ast_all.hpp"

#include <string>

class ASTRoot : public ASTNode
{
public:
    ASTRoot(ASTConstDef *const_def, ASTTypeDef *type_def, ASTVarDecl *var_decl, ASTProcFuncDecl *proc_func_decl, ASTStmt *stmt);
    ~ASTRoot();
    ASTConstDef *const_def;
    ASTTypeDef *type_def;
    ASTVarDecl *var_decl;
    ASTProcFuncDecl *proc_func_decl;
    ASTStmt *stmt;
};