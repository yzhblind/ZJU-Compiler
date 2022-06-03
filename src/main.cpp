#include "ast/ast_all.hpp"
#include "pascal.y.hpp"
#include <iostream>

ASTRoot *root_entry = nullptr;

int main()
{
    freopen("/home/ff/prgm/cp/ZJU-Compiler/test/sort.pas", "r", stdin);
    yyparse();
    return 0;
}