#pragma once

#include "ast_all.hpp"

class ASTVarAccess : public ASTNode
{
public:
	virtual ~ASTVarAccess() = 0;
	enum TypeKind {
		ID,
		INDEX,
		FIELD,
		PTR
	};
	virtual ASTVarAccess::TypeKind get_type() = 0;
};

class ASTVarAccessId : public ASTVarAccess
{
public:
	ASTVarAccessId(char *id);
	~ASTVarAccessId();
	ASTVarAccess::TypeKind get_type();
	string id;
};

class ASTVarAccessIndex : public ASTVarAccess
{
public:
	ASTVarAccessIndex(ASTExpr *start_idx);
	~ASTVarAccessIndex();
	void push_back(ASTExpr *next);
	void set_array(ASTVarAccess* arr);
	ASTVarAccess::TypeKind get_type();
	ASTVarAccess* arr;
	vector<ASTExpr*> idx;
};

class ASTVarAccessField : public ASTVarAccess
{
public:
	ASTVarAccessField(ASTVarAccess *record_var, char *id);
	~ASTVarAccessField();
	ASTVarAccess::TypeKind get_type();
	ASTVarAccess* record_var;
	string id;
};

class ASTVarAccessPointer : public ASTVarAccess
{
public:
	ASTVarAccessPointer(ASTVarAccess *ptr_var);
	~ASTVarAccessPointer();
	ASTVarAccess::TypeKind get_type();
	ASTVarAccess* ptr_var;
};


class ASTFactor : public ASTNode
{
public:
	~ASTFactor();
	enum TypeKind {
		VAR,
		CONST,
		EXPR,
		FUNC_CALL,
		AT
	};
	virtual ASTFactor::TypeKind get_type() = 0;
	void flip_not();
	bool is_not;
protected:
	ASTFactor();
};

class ASTFactorVar : public ASTFactor
{
public:
	ASTFactorVar(ASTVarAccess *var);
	~ASTFactorVar();
	ASTFactor::TypeKind get_type();
	ASTVarAccess* var;
};

class ASTFactorConst : public ASTFactor
{
public:
	ASTFactorConst(ASTConstValue *value);
	~ASTFactorConst();
	ASTFactor::TypeKind get_type();
	ASTConstValue* value;
};

class ASTFactorExpr : public ASTFactor
{
public:
	ASTFactorExpr(ASTExpr *expr);
	~ASTFactorExpr();
	ASTFactor::TypeKind get_type();
	ASTExpr* expr;
};

class ASTFactorFunc : public ASTFactor
{
public:
	ASTFactorFunc(char *id, ASTActualPara *para=nullptr);
	~ASTFactorFunc();
	ASTFactor::TypeKind get_type();
	string id;
	ASTActualPara* para;
};

class ASTFactorAt : public ASTFactor
{
public:
	ASTFactorAt(ASTVarAccess* ptr_var);
	~ASTFactorAt();
	ASTFactor::TypeKind get_type();
	ASTVarAccess* ptr_var;
};

class ASTTerm : public ASTNode 
{
public:
	enum MOP {
		AND,
		INT_DIV,
		MOD,
		MUL,
		FLT_DIV
	};
	ASTTerm(ASTFactor *left);
	ASTTerm(ASTFactor *left, MOP mop, ASTFactor* right);
	~ASTTerm();
	ASTFactor* left, *right;
	MOP mop;
};

class ASTSimpleExpr : public ASTNode
{
public:
	enum AOP {
		OR,
		ADD,
		SUB
	};
	ASTSimpleExpr(bool neg_flag, ASTTerm*left, AOP aop, ASTTerm*right);
	ASTSimpleExpr(bool neg_flag, ASTTerm* left);
	~ASTSimpleExpr();
	bool neg_flag;
	ASTTerm* left, *right;
	AOP aop;
};

class ASTExpr : public ASTNode
{
public:
	enum ROP {
		EQ,
		LT,
		GT,
		NOT_EQ,
		LE,
		GE
	};
	ASTExpr(ASTSimpleExpr* left, ROP op, ASTSimpleExpr* right);
	ASTExpr(ASTSimpleExpr* left);
	~ASTExpr();
	ASTSimpleExpr* left, *right;
	ROP rop;
};