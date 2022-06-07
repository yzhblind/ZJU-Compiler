#include "ast/ast_all.hpp"
#include "pascal.y.hpp"
#include "backend/llvm_ir.hpp"
#include <iostream>

ASTRoot* root_entry = nullptr;

int main()
{
    freopen("../../test/function.pas", "r", stdin);
    yyparse();
    cout << root_entry->stmt->get_stmt_type() << endl;
    cout << root_entry->stmt->next_stmt->get_stmt_type() << endl;
    cout << "----------------------------------------" << endl;
    IR_builder IR;
    IR.CodeGen(root_entry);
    return 0;
}