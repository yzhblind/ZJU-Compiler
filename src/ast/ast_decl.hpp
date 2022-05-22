#pragma once

#include "ast_all.hpp"

class ASTConstValue : public ASTNode
{
public:
    ASTConstValue();
    ASTConstValue(int64_t value);
    ASTConstValue(double value);
    ASTConstValue(char *str, bool isID);
    ~ASTConstValue();
    enum ConstType { INT, REAL, STRING, ID, NIL };
    ConstType value_type;
    int64_t int_value;
    double real_value;
    char *str;
};

class ASTConstDef : public ASTNode
{
public:
    ASTConstDef(ASTConstValue *value);
    ~ASTConstDef();
    void append(ASTConstDef *next);
    ASTConstDef *next_const_def;
};

class ASTTypeDef;

class ASTVarDecl;
