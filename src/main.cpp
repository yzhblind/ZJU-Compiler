#include "ast/ast_all.hpp"
#include "pascal.y.hpp"
#include "backend/llvm_ir.hpp"
#include <iostream>

extern FILE *yyin;
ASTRoot* root_entry = nullptr;

int main(int argc,char *argv[]) {
    yyin = fopen(argv[1], "r");
    // freopen("../../test/qsort.pas", "r", stdin);
    yyparse();
    

    freopen(string("IR.ll").c_str(), "w", stdout);
    IR_builder IR;
    IR.CodeGen(root_entry);

    system(("llc -filetype=obj IR.ll"));
    system(("clang++ -O3 IR.o -o run"));
    
    return 0;
}