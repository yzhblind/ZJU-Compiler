#include "ast_stmt.hpp"

ASTActualPara::ASTActualPara(ASTExpr *expr)
{
    this->expr = expr;
    next_actual_para = nullptr;
    type = EXPRESSION;
}

ASTActualPara::ASTActualPara(char *id, bool is_func)
{
    this->id = string(id);
    if (is_func)
    {
        type = PROCEDURE;
    }
    else
    {
        type = FUNCTION;
    }
}

ASTActualPara::~ASTActualPara()
{
    if (next_actual_para)
        delete next_actual_para;
    if (expr)
        delete expr;
}

ASTActualPara::TypeKind ASTActualPara::get_type()
{
    return type;
}

ASTActualPara *ASTActualPara::append(ASTActualPara *next)
{
    ASTActualPara *it = this;
    while (it->next_actual_para != nullptr)
        it = it->next_actual_para;
    it->next_actual_para = next;
    return this;
}

ASTReadPara::ASTReadPara(ASTVarAccess *start)
{
    read_para.push_back(start);
}

ASTReadPara::~ASTReadPara()
{
    for (int i = 0; i < read_para.size(); ++i)
        if (read_para[i])
            delete read_para[i];
}

void ASTReadPara::push_back(ASTVarAccess *next)
{
    read_para.push_back(next);
}

WritePack::~WritePack()
{
    if (value)
        delete value;
    if (len)
        delete len;
    if (frac_len)
        delete frac_len;
}

ASTWritePara::~ASTWritePara()
{
    for (int i = 0; i < write_para.size(); ++i)
        if (write_para[i])
            delete write_para[i];
}

ASTWritePara::ASTWritePara(WritePack *start)
{
    write_para.push_back(start);
}

void ASTWritePara::push_back(WritePack *next)
{
    write_para.push_back(next);
}

ASTStmt::ASTStmt()
{
    next_stmt = nullptr;
}

ASTStmt::~ASTStmt()
{
    if (next_stmt)
        delete next_stmt;
}

ASTStmt *ASTStmt::append(ASTStmt *next)
{
    ASTStmt *it = this;
    while (it->next_stmt != nullptr)
        it = it->next_stmt;
    it->next_stmt = next;
    return this;
}

ASTStmt::TypeKind ASTStmt::get_stmt_type()
{
    return ASTStmt::TypeKind(EMPTY);
}

ASTAssignStmt::ASTAssignStmt(ASTVarAccess *left, ASTExpr *right)
{
    this->left = left;
    this->right = right;
}

ASTAssignStmt::ASTAssignStmt(ASTExpr *right)
{
    this->left = nullptr;
    this->right = right;
}

ASTAssignStmt::~ASTAssignStmt()
{
    if (left)
        delete left;
    if (right)
        delete right;
}

ASTStmt::TypeKind ASTAssignStmt::get_stmt_type()
{
    return ASTStmt::TypeKind(ASSIGN);
}

ASTIfStmt::ASTIfStmt(ASTExpr *cond, ASTStmt *true_block, ASTStmt *false_block)
{
    this->cond = cond;
    this->true_block = true_block;
    this->false_block = false_block;
}

ASTIfStmt::~ASTIfStmt()
{
    if (cond)
        delete cond;
    if (true_block)
        delete true_block;
    if (false_block)
        delete false_block;
}

ASTStmt::TypeKind ASTIfStmt::get_stmt_type()
{
    return ASTStmt::TypeKind(IF);
}

ASTRepeatStmt::ASTRepeatStmt(ASTStmt *loop_body, ASTExpr *cond)
{
    this->loop_body = loop_body;
    this->cond = cond;
}

ASTRepeatStmt::~ASTRepeatStmt()
{
    if (loop_body)
        delete loop_body;
    if (cond)
        delete cond;
}

ASTStmt::TypeKind ASTRepeatStmt::get_stmt_type()
{
    return ASTStmt::TypeKind(REPEAT);
}

ASTWhileStmt::ASTWhileStmt(ASTExpr *cond, ASTStmt *loop_body)
{
    this->loop_body = loop_body;
    this->cond = cond;
}

ASTWhileStmt::~ASTWhileStmt()
{
    if (loop_body)
        delete loop_body;
    if (cond)
        delete cond;
}

ASTStmt::TypeKind ASTWhileStmt::get_stmt_type()
{
    return ASTStmt::TypeKind(WHILE);
}

ASTForStmt::ASTForStmt(ASTVarAccessId *loop_var, ASTExpr *init_value, ASTExpr *final_value, bool is_downto, ASTStmt *loop_body)
{
    this->loop_var = loop_var;
    this->init_value = init_value;
    this->final_value = final_value;
    this->is_downto = is_downto;
    this->loop_body = loop_body;
}

ASTForStmt::~ASTForStmt()
{
    if (loop_var)
        delete loop_var;
    if (init_value)
        delete init_value;
    if (final_value)
        delete final_value;
    if (loop_body)
        delete loop_body;
}

ASTStmt::TypeKind ASTForStmt::get_stmt_type()
{
    return ASTStmt::TypeKind(FOR);
}

ASTProcStmt::ASTProcStmt(char *id, ASTActualPara *para)
{
    if (id)
        this->id = string(id);
    this->actual_para = para;
}

ASTProcStmt::~ASTProcStmt()
{
    if (actual_para)
        delete actual_para;
}

ASTStmt::TypeKind ASTProcStmt::get_stmt_type()
{
    return ASTStmt::TypeKind(PROCEDURE_CALL);
}

ASTProcStmt::TypeKind ASTProcStmt::get_proc_type()
{
    return ASTProcStmt::TypeKind(NORMAL);
}

ASTReadStmt::ASTReadStmt(bool with_newline, ASTReadPara *para) : ASTProcStmt(nullptr)
{
    newline = with_newline;
    this->para = para;
}

ASTReadStmt::~ASTReadStmt()
{
    if (para)
        delete para;
}

ASTProcStmt::TypeKind ASTReadStmt::get_proc_type()
{
    return ASTProcStmt::TypeKind(READ);
}

ASTWriteStmt::ASTWriteStmt(bool with_newline, ASTWritePara *para) : ASTProcStmt(nullptr)
{
    newline = with_newline;
    this->para = para;
}

ASTWriteStmt::~ASTWriteStmt()
{
    if (para)
        delete para;
}

ASTProcStmt::TypeKind ASTWriteStmt::get_proc_type()
{
    return ASTProcStmt::TypeKind(WRITE);
}
