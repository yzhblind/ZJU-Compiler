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
    virtual ~ASTType() = 0;
    enum TypeKind
    {
        ID,
        SUBRANGE,
        ARRAY,
        RECORD,
        POINTER
    };
    virtual ASTType::TypeKind get_type() = 0;
};

class ASTTypeId : public ASTType
{
public:
    ASTTypeId(char *id);
    ~ASTTypeId();
    ASTType::TypeKind get_type();
    string id;
};

class ASTTypeSubrange : public ASTType
{
public:
    ASTTypeSubrange(char *id);
    ASTTypeSubrange(ASTConstValue *left, ASTConstValue *right);
    ~ASTTypeSubrange();
    ASTType::TypeKind get_type();
    bool is_range_id;
    string range_id;
    ASTConstValue *left, *right;
};

class ASTTypeStructure : public ASTType
{
public:
    ASTTypeStructure();
    virtual ~ASTTypeStructure() = 0;
    virtual ASTType::TypeKind get_type() = 0;
    void set_packed_flag();
    bool packed_flag;
};

class ASTTypeArray : public ASTTypeStructure
{
public:
    ASTTypeArray();
    ~ASTTypeArray();
    ASTType::TypeKind get_type();
    void set_element_type(ASTType *type);
    void push_index(ASTTypeSubrange *idx);
    vector<ASTTypeSubrange *> index;
    ASTType *element_type;
};

class ASTTypeRecord : public ASTTypeStructure
{
public:
    ~ASTTypeRecord();
    ASTType::TypeKind get_type();
    void push_var_decl(ASTVarDecl *decl);
    vector<ASTVarDecl *> var_decl_list;
};

class ASTTypePointer : public ASTType
{
public:
    ASTTypePointer(ASTType *type);
    ~ASTTypePointer();
    ASTType::TypeKind get_type();
    ASTType *pointer_type;
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
    ASTVarDecl(vector<char *> &id, ASTType *type);
    ~ASTVarDecl();
    ASTVarDecl *append(ASTVarDecl *next);
    vector<string> id_list;
    ASTType *var_type;
    ASTVarDecl *next_var_decl;
};
