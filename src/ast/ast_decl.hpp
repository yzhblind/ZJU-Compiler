#pragma once

#include "ast_all.hpp"

#include <string>
using std::string;

#include <vector>
using std::vector;

class ASTConstValue : public ASTNode
{
public:
    ASTConstValue();
    ASTConstValue(int64_t value);
    ASTConstValue(double value);
    ASTConstValue(char *str, bool isID);
    ~ASTConstValue();
    enum ConstType
    {
        INT,
        REAL,
        STRING,
        ID,
        NIL
    };
    ConstType value_type;
    int64_t int_value;
    double real_value;
    string str;
};

class ASTConstDef : public ASTNode
{
public:
    ASTConstDef(char *id, ASTConstValue *value);
    ~ASTConstDef();
    ASTConstDef *append(ASTConstDef *next);
    string id;
    ASTConstValue *value;
    ASTConstDef *next_const_def;
};

class ASTVarDecl;

class ASTType : public ASTNode
{
public:
    ~ASTType();
    enum TypeKind
    {
        ID,
        SUBRANGE,
        ARRAY,
        RECORD,
        POINTER
    };
    virtual TypeKind get_type() = 0;
};

class ASTTypeSubrange : public ASTType
{
public:
    TypeKind get_type();
};

class ASTTypeStructure : public ASTType
{
public:
    virtual TypeKind get_type() = 0;
    virtual void set_packed_flag() = 0;
    bool packed_flag;
};

class ASTTypeArray : public ASTTypeStructure
{
public:
    TypeKind get_type();
};

class ASTTypeRecord : public ASTTypeStructure
{
public:
    TypeKind get_type();
};

class ASTTypePointer : public ASTType
{
public:
    TypeKind get_type();
};

class ASTTypeDef : public ASTNode
{
public:
    ASTTypeDef(char *id, ASTType *type);
    ~ASTTypeDef();
    ASTTypeDef *append(ASTTypeDef *next);
    string id;
    ASTType *type;
    ASTTypeDef *next_type_def;
};

class ASTVarDecl : public ASTNode
{
public:
};
