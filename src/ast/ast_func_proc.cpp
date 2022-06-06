#include "ast_func_proc.hpp"

ASTParameter::ASTParameter(ASTVarDecl *var_decl, bool is_ref)
{
    if (is_ref)
    {
        this->var_decl;
        proc_func = nullptr;
        type = REF_VARIABLE;
    }
    else
    {
        this->var_decl;
        proc_func = nullptr;
        type = VARIABLE;
    }
    next_para = nullptr;
}

ASTParameter::ASTParameter(ASTProcDecl *proc_decl)
{
    this->proc_func = proc_func;
    var_decl = nullptr;
    type = PROCEDURE;
    next_para = nullptr;
}

ASTParameter::ASTParameter(ASTFuncDecl *func_decl)
{
    this->proc_func = func_decl;
    var_decl = nullptr;
    type = PROCEDURE;
    next_para = nullptr;
}

ASTParameter::~ASTParameter()
{
    if (var_decl)
        delete var_decl;
    if (proc_func)
        delete proc_func;
    if (next_para)
        delete next_para;
}

ASTParameter *ASTParameter::append(ASTParameter *next)
{
    ASTParameter *it = this;
    while (it->next_para != nullptr)
        it = it->next_para;
    it->next_para = next;
    return this;
}

ASTParameter::TypeKind ASTParameter::get_type()
{
    return type;
}

ASTProcFuncBlock::ASTProcFuncBlock(ASTConstDef *const_def, ASTTypeDef *type_def, ASTVarDecl *var_decl, ASTStmt *stmt)
{
    this->const_def = const_def;
    this->type_def = type_def;
    this->var_decl = var_decl;
    this->stmt = stmt;
}

ASTProcFuncBlock::~ASTProcFuncBlock()
{
    if (const_def)
        delete const_def;
    if (type_def)
        delete type_def;
    if (var_decl)
        delete var_decl;
    if (stmt)
        delete stmt;
}

ASTProcFuncDecl::~ASTProcFuncDecl()
{
    if (para)
        delete para;
    if (block)
        delete block;
    if (next_proc_func_decl)
        delete next_proc_func_decl;
}

ASTProcFuncDecl *ASTProcFuncDecl::append(ASTProcFuncDecl *next)
{
    ASTProcFuncDecl *it;
    while (it->next_proc_func_decl != nullptr)
        it = it->next_proc_func_decl;
    it->next_proc_func_decl = next;
    return this;
}

void ASTProcFuncDecl::set_forward()
{
    is_forward = true;
}

void ASTProcFuncDecl::set_block(ASTProcFuncBlock *block)
{
    this->block = block;
}

ASTProcDecl::ASTProcDecl(char *id, ASTParameter *para)
{
    this->id = string(id);
    this->para = para;
    is_forward = false;
    block = nullptr;
}

ASTProcDecl::~ASTProcDecl()
{
}

ASTProcFuncDecl::TypeKind ASTProcDecl::get_type()
{
    return PROCEDURE;
}

ASTFuncDecl::ASTFuncDecl(char *id, ASTParameter *para, char *result_id)
{
    this->id = string(id);
    this->para = para;
    ret_type_id = string(result_id);
    is_forward = false;
    block = nullptr;
}

ASTFuncDecl::~ASTFuncDecl()
{
}

ASTProcFuncDecl::TypeKind ASTFuncDecl::get_type()
{
    return FUNCTION;
}
