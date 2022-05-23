#include "ast_decl.hpp"

ASTConstValue::ASTConstValue()
{
    value_type = NIL;
}

ASTConstValue::ASTConstValue(int64_t value)
{
    value_type = INT;
    int_value = value;
}

ASTConstValue::ASTConstValue(double value)
{
    value_type = REAL;
    real_value = value;
}

ASTConstValue::ASTConstValue(char *str, bool isID)
{
    if (isID == true)
    {
        value_type = ID;
        this->str = string(str);
    }
    else
    {
        value_type = STRING;
        this->str = string(str + 1);
        this->str.erase(this->str.length() - 1);
    }
}

ASTConstValue::~ASTConstValue()
{
    // Nothing
}

ASTConstDef::ASTConstDef(char *id, ASTConstValue *value)
{
    this->id = string(id);
    this->value = value;
    next_const_def = nullptr;
}

ASTConstDef::~ASTConstDef()
{
    delete value;
}

ASTConstDef *ASTConstDef::append(ASTConstDef *next)
{
    next_const_def = next;
    return this;
}