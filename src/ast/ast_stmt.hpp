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
    };
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

class ASTWritePara : public ASTNode
{
public:
    ~ASTWritePara();
    struct WritePack
    {
        WritePack(ASTExpr *value, ASTExpr *len = nullptr, ASTExpr *frac_len = nullptr) : value(value), len(len), frac_len(frac_len) {}
        ~WritePack()
        {
            if (value)
                delete value;
            if (len)
                delete len;
            if (frac_len)
                delete frac_len;
        }
        ASTExpr *value;
        ASTExpr *len;
        ASTExpr *frac_len;
    };
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
};

class ASTIfStmt : public ASTStmt
{
public:
    ASTIfStmt(ASTExpr *cond, ASTStmt *true_block, ASTStmt *false_block);
    ~ASTIfStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
};

class ASTRepeatStmt : public ASTStmt
{
public:
    ASTRepeatStmt(ASTStmt *loop_body, ASTExpr *cond);
    ~ASTRepeatStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
};

class ASTWhileStmt : public ASTStmt
{
public:
    ASTWhileStmt(ASTExpr *cond, ASTStmt *loop_body);
    ~ASTWhileStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
};

class ASTForStmt : public ASTStmt
{
public:
    ASTForStmt(char *loop_var, ASTExpr *init, ASTExpr *final, bool is_downto, ASTStmt *loop_body);
    ~ASTForStmt();
    virtual ASTStmt::TypeKind get_stmt_type();
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
};

class ASTWriteStmt : public ASTProcStmt
{
public:
    ASTWriteStmt(bool with_newline, ASTWritePara *para);
    ~ASTWriteStmt();
    ASTProcStmt::TypeKind get_proc_type();
};
