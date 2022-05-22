#include "ast_prgm.hpp"

ASTRoot::ASTRoot(ASTConstDef *const_def, ASTTypeDef *type_def, ASTVarDecl *var_decl, ASTProcFuncDecl *proc_func_decl, ASTStmt *stmt)
{
    this->const_def = const_def;
    this->type_def = type_def;
    this->var_decl = var_decl;
    this->proc_func_decl = proc_func_decl;
    this->stmt = stmt;
}

ASTRoot::~ASTRoot()
{
    delete const_def;
    delete type_def;
    delete var_decl;
    delete proc_func_decl;
    delete stmt;
}
