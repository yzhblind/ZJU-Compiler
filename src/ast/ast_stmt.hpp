#pragma once

#include "ast_all.hpp"

class ASTActualPara : public ASTNode
{
public:
    ASTActualPara(ASTExpr *expr);
    ASTActualPara(char *id, bool is_func);
    ~ASTActualPara();
    enum TypeKind
    {
        EXPRESSION,
        PROCEDURE,
        FUNCTION
    } type;
    ASTActualPara::TypeKind get_type();
    ASTActualPara *append(ASTActualPara *next);
    ASTActualPara *next_actual_para;
    ASTExpr *expr;
    string id;
};

class ASTReadPara : public ASTNode
{
public:
    ASTReadPara(ASTVarAccess *start);
    ~ASTReadPara();
    void push_back(ASTVarAccess *next);
    vector<ASTVarAccess *> read_para;
};

struct WritePack
{
    WritePack(ASTExpr *value, ASTExpr *len = nullptr, ASTExpr *frac_len = nullptr) : value(value), len(len), frac_len(frac_len) {}
    ~WritePack();
    ASTExpr *value;
    ASTExpr *len;
    ASTExpr *frac_len;
};

class ASTWritePara : public ASTNode
{
public:
    ~ASTWritePara();
    ASTWritePara(WritePack *start);
    void push_back(WritePack *next);
    vector<WritePack *> write_para;
};

class ASTStmt : public ASTNode
{
public:
    ASTStmt();
    ~ASTStmt();
    ASTStmt *append(ASTStmt *next);
    ASTStmt *next_stmt;
    enum TypeKind
    {
        EMPTY,
        ASSIGN,
        PROCEDURE_CALL,
        IF,
        REPEAT,
        WHILE,
        FOR
    };
    virtual ASTStmt::TypeKind get_stmt_type();
};

class ASTAssignStmt : public ASTStmt
{
public:
    ASTAssignStmt(ASTVarAccess *left, ASTExpr *right);
    ASTAssignStmt(ASTExpr *right);
    ~ASTAssignStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
    // 如果left是nullptr则代表是函数返回赋值语句
    ASTVarAccess *left;
    ASTExpr *right;
};

class ASTIfStmt : public ASTStmt
{
public:
    ASTIfStmt(ASTExpr *cond, ASTStmt *true_block, ASTStmt *false_block);
    ~ASTIfStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
    ASTExpr *cond;
    ASTStmt *true_block;
    ASTStmt *false_block;
};

class ASTRepeatStmt : public ASTStmt
{
public:
    ASTRepeatStmt(ASTStmt *loop_body, ASTExpr *cond);
    ~ASTRepeatStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
    ASTStmt *loop_body;
    ASTExpr *cond;
};

class ASTWhileStmt : public ASTStmt
{
public:
    ASTWhileStmt(ASTExpr *cond, ASTStmt *loop_body);
    ~ASTWhileStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
    ASTExpr *cond;
    ASTStmt *loop_body;
};

class ASTForStmt : public ASTStmt
{
public:
    ASTForStmt(ASTVarAccessId *loop_var, ASTExpr *init_value, ASTExpr *final_value, bool is_downto, ASTStmt *loop_body);
    ~ASTForStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
    ASTVarAccessId *loop_var;
    ASTExpr *init_value;
    ASTExpr *final_value;
    bool is_downto;
    ASTStmt *loop_body;
};

class ASTProcStmt : public ASTStmt
{
public:
    ASTProcStmt(char *id, ASTActualPara *para = nullptr);
    ~ASTProcStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
    enum TypeKind
    {
        NORMAL,
        READ,
        WRITE
    };
    virtual ASTProcStmt::TypeKind get_proc_type();
    string id;
    ASTActualPara *actual_para;
};

class ASTReadStmt : public ASTProcStmt
{
public:
    ASTReadStmt(bool with_newline, ASTReadPara *para);
    ~ASTReadStmt();
    ASTProcStmt::TypeKind get_proc_type();
    bool newline;
    ASTReadPara *para;
};

class ASTWriteStmt : public ASTProcStmt
{
public:
    ASTWriteStmt(bool with_newline, ASTWritePara *para);
    ~ASTWriteStmt();
    ASTProcStmt::TypeKind get_proc_type();
    bool newline;
    ASTWritePara *para;
};
