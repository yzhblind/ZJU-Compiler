#include "ast/ast_all.hpp"
#include "pascal.y.hpp"
#include "backend/llvm_ir.hpp"
#include <iostream>

ASTRoot* root_entry = nullptr;

int main()
{
    freopen("/home/l1ll5/ZJU-Compiler/test/aplusb.pas", "r", stdin);
    yyparse();
    IR_builder IR;
    IR.CodeGen(root_entry);
    return 0;
}