#include "ast/ast_all.hpp"
#include"pascal.y.hpp"

ASTRoot *root_entry = nullptr;

int main()
{
    yyparse();
    return 0;
}