#include "ast_expr.hpp"

ASTVarAccessId::ASTVarAccessId(char* id)
{
	this->id = string(id);
}

ASTVarAccessId::~ASTVarAccessId()
{
}

ASTVarAccess::TypeKind ASTVarAccessId::get_type()
{
	return ASTVarAccess::TypeKind(ID);
}

ASTVarAccessIndex::ASTVarAccessIndex(ASTExpr* start_idx)
{
	arr = nullptr;
	idx.push_back(start_idx);
}

ASTVarAccessIndex::~ASTVarAccessIndex()
{
	for (int i = 0; i < idx.size(); ++i)
		if (idx[i])
			delete idx[i];
}

void ASTVarAccessIndex::push_back(ASTExpr* next)
{
	idx.push_back(next);
}

void ASTVarAccessIndex::set_array(ASTVarAccess* arr)
{
	this->arr = arr;
}

ASTVarAccess::TypeKind ASTVarAccessIndex::get_type()
{
	return ASTVarAccess::TypeKind(INDEX);
}

ASTVarAccessField::ASTVarAccessField(ASTVarAccess* record_var, char* id)
{
	this->record_var = record_var;
	this->id = string(id);
}

ASTVarAccessField::~ASTVarAccessField()
{
	if (record_var)
		delete record_var;
}

ASTVarAccess::TypeKind ASTVarAccessField::get_type()
{
	return ASTVarAccess::TypeKind(FIELD);
}

ASTVarAccessPointer::ASTVarAccessPointer(ASTVarAccess* ptr_var)
{
	this->ptr_var = ptr_var;
}

ASTVarAccessPointer::~ASTVarAccessPointer()
{
	if (ptr_var)
		delete ptr_var;
}

ASTVarAccess::TypeKind ASTVarAccessPointer::get_type()
{
	return ASTVarAccess::TypeKind(PTR);
}

ASTFactor::~ASTFactor()
{
}

void ASTFactor::flip_not()
{
	is_not = !is_not;
}

ASTFactor::ASTFactor()
{
	is_not = false;
}

ASTFactorVar::ASTFactorVar(ASTVarAccess* var)
{
	this->var = var;
}

ASTFactorVar::~ASTFactorVar()
{
	if (var)
		delete var;
}

ASTFactor::TypeKind ASTFactorVar::get_type()
{
	return ASTFactor::TypeKind(VAR);
}

ASTFactorConst::ASTFactorConst(ASTConstValue* value)
{
	this->value = value;
}

ASTFactorConst::~ASTFactorConst()
{
	if (value)
		delete value;
}

ASTFactor::TypeKind ASTFactorConst::get_type()
{
	return ASTFactor::TypeKind(CONST);
}

ASTFactorExpr::ASTFactorExpr(ASTExpr* expr)
{
	this->expr = expr;
}

ASTFactorExpr::~ASTFactorExpr()
{
	if (expr)
		delete expr;
}

ASTFactor::TypeKind ASTFactorExpr::get_type()
{
	return ASTFactor::TypeKind(EXPR);
}

ASTFactorFunc::ASTFactorFunc(char* id, ASTActualPara* para)
{
	this->id = string(id);
	this->para = para;
}

ASTFactorFunc::~ASTFactorFunc()
{
	if (para)
		delete para;
}

ASTFactor::TypeKind ASTFactorFunc::get_type()
{
	return ASTFactor::TypeKind(FUNC_CALL);
}

ASTFactorAt::ASTFactorAt(ASTVarAccess* ptr_var)
{
	this->ptr_var = ptr_var;
}

ASTFactorAt::~ASTFactorAt()
{
	if (ptr_var)
		delete ptr_var;
}

ASTFactor::TypeKind ASTFactorAt::get_type()
{
	return ASTFactor::TypeKind(AT);
}

ASTTerm::ASTTerm(ASTFactor* left)
{
	this->left = left;
	this->right = nullptr;
}

ASTTerm::ASTTerm(ASTFactor* left, MOP mop, ASTFactor* right)
{
	this->left = left;
	this->mop = mop;
	this->right = right;
}

ASTTerm::~ASTTerm()
{
	if (left)
		delete left;
	if (right)
		delete right;
}

ASTSimpleExpr::ASTSimpleExpr(bool neg_flag, ASTTerm* left, AOP aop, ASTTerm* right)
{
	this->neg_flag = neg_flag;
	this->left = left;
	this->aop = aop;
	this->right = right;
}

ASTSimpleExpr::ASTSimpleExpr(bool neg_flag, ASTTerm* left)
{
	this->neg_flag = neg_flag;
	this->left = left;
	this->right = nullptr;
}

ASTSimpleExpr::~ASTSimpleExpr()
{
	if (left)
		delete left;
	if (right)
		delete right;
}

ASTExpr::ASTExpr(ASTSimpleExpr* left, ROP rop, ASTSimpleExpr* right)
{
	this->left = left;
	this->rop = rop;
	this->right = right;
}

ASTExpr::ASTExpr(ASTSimpleExpr* left)
{
	this->left = left;
	this->right = nullptr;
}

ASTExpr::~ASTExpr()
{
	if (left)
		delete left;
	if (right)
		delete right;
}

ASTVarAccess::~ASTVarAccess()
{
}
