#pragma once

#include "ast_all.hpp"

#include <string>
// 根节点，整个程序的入口
class ASTRoot : public ASTNode
{
public:
    ASTRoot(ASTConstDef *const_def, ASTTypeDef *type_def, ASTVarDecl *var_decl, ASTProcFuncDecl *proc_func_decl, ASTStmt *stmt);
    ~ASTRoot();
    // 常量定义
    ASTConstDef *const_def;
    // 类型定义
    ASTTypeDef *type_def;
    // 变量声明
    ASTVarDecl *var_decl;
    // 可能为nullptr
    ASTProcFuncDecl *proc_func_decl;
    // 程序主体部分
    ASTStmt *stmt;
};