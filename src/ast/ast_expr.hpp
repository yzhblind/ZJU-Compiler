#pragma once

#include "ast_all.hpp"

class ASTVarAccess : public ASTNode
{
public:
	~ASTVarAccess();
};

class ASTVarAccessId : public ASTVarAccess
{
public:
	ASTVarAccessId(char *id);
	~ASTVarAccessId();
};

class ASTVarAccessIndex : public ASTVarAccess
{
public:
	ASTVarAccessIndex(ASTExpr *start_idx);
	~ASTVarAccessIndex();
	void push_back(ASTExpr *next);
	void set_array(ASTVarAccess* arr);
	ASTVarAccess* arr;
	vector<ASTExpr*> idx;
};

class ASTVarAccessField : public ASTVarAccess
{
public:
	ASTVarAccessField(ASTVarAccess *record_var, char *id);
	~ASTVarAccessField();
};

class ASTVarAccessPointer : public ASTVarAccess
{
public:
	ASTVarAccessPointer(ASTVarAccess *ptr_var);
	~ASTVarAccessPointer();
};


class ASTFactor : public ASTNode
{
public:
	ASTFactor();
	~ASTFactor();
	bool is_not;
	void flip_not();
};

class ASTFactorVar : public ASTFactor
{
public:
	ASTFactorVar(ASTVarAccess *var);
	~ASTFactorVar();
};

class ASTFactorConst : public ASTFactor
{
public:
	ASTFactorConst(ASTConstValue *value);
	~ASTFactorConst();
};

class ASTFactorExpr : public ASTFactor
{
public:
	ASTFactorExpr(ASTExpr *expr);
	~ASTFactorExpr();
};

class ASTFactorFunc : public ASTFactor
{
public:
	ASTFactorFunc(char *id, ASTActualPara *para=nullptr);
	~ASTFactorFunc();
};

class ASTFactorAt : public ASTFactor
{
public:
	ASTFactorAt(ASTVarAccess* ptr_var);
	~ASTFactorAt();
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
};