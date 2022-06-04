#pragma once

#include "ast_all.hpp"

#include <string>
using std::string;

#include <vector>
using std::vector;
// 常量值节点，因为各种常量值所占空间都很小，选择直接将其放置于一个节点内
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
        INT, //整数，当int64处理
        REAL, //浮点数，当double处理
        STRING, //字符串
        ID, //标识符
        NIL //空指针
    };
    // 表示当前常量值的类型
    ConstType value_type;
    int64_t int_value;
    double real_value;
    string str;
};
// 常量定义节点，每个常量定义节点的结构都是将某个常量值定义为标识符
class ASTConstDef : public ASTNode
{
public:
    ASTConstDef(char *id, ASTConstValue *value);
    ~ASTConstDef();
    ASTConstDef *append(ASTConstDef *next);
    // 标识符
    string id;
    // 常量值
    ASTConstValue *value;
    // 如果不为nullptr说明当前常量定义区还未结束
    ASTConstDef *next_const_def;
};

// 定义类型节点，类型类是纯虚类，只作为指针使用
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
    // 表示当前子类具体是什么类型，和dynamic cast用于判断类型时用途重合但会更方便一点？
    virtual ASTType::TypeKind get_type() = 0;
};

class ASTTypeId : public ASTType
{
public:
    ASTTypeId(char *id);
    ~ASTTypeId();
    ASTType::TypeKind get_type();
    // 类型本身可以是另一个类型标识符
    string id;
};
// range类型，在我们简化过的pascal的语法中唯一的作用是定义数组下标范围
class ASTTypeSubrange : public ASTType
{
public:
    ASTTypeSubrange(char *id);
    ASTTypeSubrange(ASTConstValue *left, ASTConstValue *right);
    ~ASTTypeSubrange();
    ASTType::TypeKind get_type();
    // range有两种，一种是代表range的标识符，一种是直接的定义
    // 前者最终还是要归结到后者
    bool is_range_id;
    // true：使用string读取标识符
    string range_id;
    // false：直接读取上下界，注意检查常量值的类型是否合法
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
