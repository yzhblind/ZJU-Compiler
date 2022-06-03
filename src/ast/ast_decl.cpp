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
    if (value)
        delete value;
    if (next_const_def)
        delete next_const_def;
}

ASTConstDef *ASTConstDef::append(ASTConstDef *next)
{
    next_const_def = next;
    return this;
}

ASTType::~ASTType()
{
}

ASTTypeId::ASTTypeId(char *id)
{
    this->id = string(id);
}

ASTTypeId::~ASTTypeId()
{
    // Nothing
}

ASTType::TypeKind ASTTypeId::get_type()
{
    return ID;
}

ASTTypeSubrange::ASTTypeSubrange(char *id)
{
    is_range_id = true;
    range_id = string(id);
    this->left = this->right = nullptr;
}

ASTTypeSubrange::ASTTypeSubrange(ASTConstValue *left, ASTConstValue *right)
{
    is_range_id = false;
    this->left = left;
    this->right = right;
}

ASTTypeSubrange::~ASTTypeSubrange()
{
    if (left)
        delete left;
    if (right)
        delete right;
}

ASTType::TypeKind ASTTypeSubrange::get_type()
{
    return SUBRANGE;
}

ASTTypeStructure::ASTTypeStructure()
{
    packed_flag = false;
}

ASTTypeStructure::~ASTTypeStructure()
{
}

void ASTTypeStructure::set_packed_flag()
{
    packed_flag = true;
}

ASTTypeArray::ASTTypeArray()
{
    element_type = nullptr;
}

ASTTypeArray::~ASTTypeArray()
{
    for (int i = 0; i < index.size(); ++i)
        delete index[i];
    if (element_type)
        delete element_type;
}

ASTType::TypeKind ASTTypeArray::get_type()
{
    return ARRAY;
}

void ASTTypeArray::set_element_type(ASTType *type)
{
    element_type = type;
}

void ASTTypeArray::push_index(ASTTypeSubrange *idx)
{
    index.push_back(idx);
}

ASTTypeRecord::~ASTTypeRecord()
{
    for (int i = 0; i < var_decl_list.size(); ++i)
        delete var_decl_list[i];
}

ASTType::TypeKind ASTTypeRecord::get_type()
{
    return RECORD;
}

void ASTTypeRecord::push_var_decl(ASTVarDecl *decl)
{
    var_decl_list.push_back(decl);
}

ASTTypePointer::ASTTypePointer(ASTType *type)
{
    pointer_type = type;
}

ASTTypePointer::~ASTTypePointer()
{
    if (pointer_type)
        delete pointer_type;
}

ASTType::TypeKind ASTTypePointer::get_type()
{
    return POINTER;
}

ASTTypeDef::ASTTypeDef(char *id, ASTType *type)
{
    this->id = string(id);
    this->type = type;
    next_type_def = nullptr;
}

ASTTypeDef::~ASTTypeDef()
{
    if (type)
        delete type;
    if (next_type_def)
        delete next_type_def;
}

ASTTypeDef *ASTTypeDef::append(ASTTypeDef *next)
{
    next_type_def = next;
    return this;
}

ASTVarDecl::ASTVarDecl(vector<char *> &id, ASTType *type)
{
    for (int i = 0; i < id.size(); ++i)
        id_list.emplace_back(string(id[i]));
    var_type = type;
    next_var_decl = nullptr;
}

ASTVarDecl::~ASTVarDecl()
{
    if (var_type)
        delete var_type;
    if (next_var_decl)
        delete next_var_decl;
}

ASTVarDecl *ASTVarDecl::append(ASTVarDecl *next)
{
    next_var_decl = next;
    return this;
}
