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
    if (const_def)
        delete const_def;
    if (type_def)
        delete type_def;
    if (var_decl)
        delete var_decl;
    if (proc_func_decl)
        delete proc_func_decl;
    if (stmt)
        delete stmt;
}
