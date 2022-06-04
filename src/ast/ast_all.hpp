#pragma once

class ASTNode;

class ASTConstValue;
class ASTConstDef;
class ASTType;
class ASTTypeId;
class ASTTypeSubrange;
class ASTTypeStructure;
class ASTTypeArray;
class ASTTypeRecord;
class ASTTypePointer;
class ASTTypeDef;
class ASTVarDecl;

class ASTVarAccess;
class ASTVarAccessId;
class ASTVarAccessIndex;
class ASTVarAccessField;
class ASTVarAccessPointer;
class ASTFactor;
class ASTFactorVar;
class ASTFactorConst;
class ASTFactorExpr;
class ASTFactorFunc;
class ASTFactorAt;
class ASTTerm;
class ASTSimpleExpr;
class ASTExpr;

class ASTParameter;
class ASTProcFuncBlock;
class ASTProcFuncDecl;
class ASTProcDecl;
class ASTFuncDecl;

class ASTRoot;

class ASTActualPara;
class ASTReadPara;
class ASTWritePara;
class ASTStmt;
class ASTAssignStmt;
class ASTIfStmt;
class ASTRepeatStmt;
class ASTWhileStmt;
class ASTForStmt;
class ASTProcStmt;
class ASTReadStmt;
class ASTWriteStmt;

#include <string>
#include <vector>
using std::string;
using std::vector;

#include"ast_base.hpp"
#include"ast_decl.hpp"
#include"ast_expr.hpp"
#include"ast_func_proc.hpp"
#include"ast_prgm.hpp"
#include"ast_stmt.hpp"
