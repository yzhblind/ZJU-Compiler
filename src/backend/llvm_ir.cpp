#include "llvm_ir.hpp"

IR_builder::IR_builder() {
    Owner = make_unique<Module>("test", Context);
    M = Owner.get();
}
IR_builder::~IR_builder() {

}

void IR_builder::CodeGen(ASTRoot* root) {
    //cout << "!" << endl;
    Function* Main = Function::Create(FunctionType::get(Type::getInt64Ty(Context), { Type::getInt64Ty(Context) }, false), Function::ExternalLinkage, "Main", M);
    BasicBlock* MainB = BasicBlock::Create(Context, "Global", Main);
    IRBuilder<> builder(MainB);
    //因为只有两层，就不dfs了
    //首先考虑全局常量

    map<string, Value*> Value_map;

    function<Value* (ASTConstValue*)> get_constant_value = [&](ASTConstValue* val) {
        auto type = val->value_type;
        using T = ASTConstValue::ConstType;
        if (type == T::INT) {
            return builder.getInt64(val->int_value);
        }
        else if (type == T::REAL) {
            // 好像没找到
            //return builder.getDouble(val->real_value);
        }
        else if (type == T::STRING) {
            //return builder.getString();
            //还没找到
        }
        else if (type == T::ID) {
            //????
        }
        else {
            //????
            assert(type == T::NIL);
        }
    };

    function<Value* (ASTVarAccess*)> var_access = [&](ASTVarAccess* var) {
        auto type = var->get_type();
        using T = ASTVarAccess::TypeKind;
        if (type == T::ID) {
            return Value_map[dynamic_cast<ASTVarAccessId*>(var)->id];
        }
        else if (type == T::INDEX) {

        }
        else if (type == T::FIELD) {

        }
        else if (type == T::PTR) {

        }
    };

    function<Value* (ASTType*)>build_var = [&](ASTType* val) {
        auto type = val->get_type();
        using T = ASTType::TypeKind;
        Value* ret;
        if (type == T::ID) {
            auto ret = dynamic_cast<ASTTypeId*>(val);
            if (ret->id == "integer") {
                Value* ret = builder.CreateAlloca(Type::getInt64Ty(Context));
                return ret;
            }
            else if (ret->id == "real") {
                Value* ret = builder.CreateAlloca(Type::getDoubleTy(Context));
                return ret;
            }
        } //...
    };

    function<Value* (ASTExpr*)> get_exp_value = [&](ASTExpr* expr) {

        function<Value* (ASTSimpleExpr*)> get_simple = [&](ASTSimpleExpr* expr) {

            function<Value* (ASTTerm*)> get_term = [&](ASTTerm* expr) {

                function<Value* (ASTFactor*)> get_factor = [&](ASTFactor* expr) {
                    using T = ASTFactor::TypeKind;
                    auto type = expr->get_type();

                    if (type == T::VAR) {
                        Value* ret = builder.CreateLoad(Type::getInt64Ty(Context), var_access(dynamic_cast<ASTFactorVar*>(expr)->var));
                        return ret;
                    }
                    else if (type == T::CONST) {
                        return get_constant_value(dynamic_cast<ASTFactorConst*>(expr)->value);
                    }
                    else if (type == T::EXPR) {

                    }
                    else if (type == T::FUNC_CALL) {

                    }
                    else {
                        assert(type == T::AT);
                    }
                };

                if (expr->right) {
                    auto type = expr->mop;
                    using T = ASTTerm::MOP;
                    Value* ret;
                    if (type == T::AND)
                        ret = builder.CreateAnd(get_factor(expr->left), get_factor(expr->right));
                    else if (type == T::INT_DIV)
                        ret = builder.CreateSDiv(get_factor(expr->left), get_factor(expr->right));
                    else if (type == T::MOD)
                        ret = builder.CreateSRem(get_factor(expr->left), get_factor(expr->right));
                    else if (type == T::MUL)
                        ret = builder.CreateMul(get_factor(expr->left), get_factor(expr->right));
                    else if (type == T::FLT_DIV)
                        ret = builder.CreateFDiv(get_factor(expr->left), get_factor(expr->right));
                }
                return get_factor(expr->left);
            };

            if (expr->right) {
                //do sth
                auto type = expr->aop;
                using T = ASTSimpleExpr::AOP;
                Value* ret;

                if (type == T::ADD)
                    ret = builder.CreateAdd(get_term(expr->left), get_term(expr->right));
                else if (type == T::SUB)
                    ret = builder.CreateSub(get_term(expr->left), get_term(expr->right));
                else if (type == T::OR)
                    ret = builder.CreateOr(get_term(expr->left), get_term(expr->right));
                return ret;
            }

            return get_term(expr->left);
        };

        if (expr->right) {
            //do sth
            auto type = expr->rop;
            using T = ASTExpr::ROP;
            Value* ret;
            //这里还没有区分整数和浮点数

            if (type == T::EQ)
                ret = builder.CreateICmpEQ(get_simple(expr->left), get_simple(expr->right));
            else if (type == T::LT)
                ret = builder.CreateICmpSLT(get_simple(expr->left), get_simple(expr->right));
            else if (type == T::GT)
                ret = builder.CreateICmpSGT(get_simple(expr->left), get_simple(expr->right));
            else if (type == T::NOT_EQ)
                ret = builder.CreateICmpNE(get_simple(expr->left), get_simple(expr->right));
            else if (type == T::LE)
                ret = builder.CreateICmpSLE(get_simple(expr->left), get_simple(expr->right));
            else if (type == T::GE)
                ret = builder.CreateICmpSGE(get_simple(expr->left), get_simple(expr->right));

            return get_simple(expr->left);
        }
        return get_simple(expr->left);
    };

    function<void(const string&, ASTStmt*)> Stmt_Gen = [&](const string& pref, ASTStmt* stmt) {
        auto type = stmt->get_stmt_type();
        cout << type << endl;
        using T = ASTStmt::TypeKind;
        if (type == T::EMPTY) {
            //do nothing
            return;
        }
        else if (type == T::ASSIGN) {
            auto it = dynamic_cast<ASTAssignStmt*>(stmt);
            Value* right = get_exp_value(it->right);
            Value* left = var_access(it->left);
            builder.CreateStore(left, right);
        }
        else if (type == T::PROCEDURE_CALL) {

        }
        else if (type == T::IF) {
            auto it = dynamic_cast<ASTIfStmt*>(stmt);

            BasicBlock* True_Block = BasicBlock::Create(Context, pref + "true_block", Main);
            builder.SetInsertPoint(True_Block);
            Stmt_Gen(pref + "IF_True", it->true_block);

            BasicBlock* False_Block = BasicBlock::Create(Context, pref + "false_block", Main);
            builder.SetInsertPoint(False_Block);
            Stmt_Gen(pref + "IF_False", it->false_block);

            builder.CreateCondBr(get_exp_value(it->cond), True_Block, False_Block);
        }
    };

    {
        auto it = root->const_def;
        while (it) {
            Value_map[it->id] = get_constant_value(it->value);
            it = it->next_const_def;
        }
    }
    //type_def是干什么用的?
    //typedef/using in cpp?
    //好像懂了
    {
        auto it = root->type_def;
        while (it) {
            //do sth
            it = it->next_type_def;
        }
    }

    {
        auto it = root->var_decl;
        while (it) {
            //do sth
            for (string str : it->id_list) {
                Value_map[str] = build_var(it->var_type);
            }
            it = it->next_var_decl;
        }
    }

    {
        auto it = root->proc_func_decl;
        //这里构建函数 好像应该把上面的部分封装起来，返回一个Function*

        //dosth
    }

    int cnt = 0;

    //BasicBlock* Fun = BasicBlock::Create(Context, "Fun", Main);

    {
        auto it = root->stmt;
        //程序主体部分
        while (it) {
            cout << it->get_stmt_type() << endl;
            //Stmt_Gen(to_string(cnt += 1), it);
            it = it->next_stmt;
        }
    }
    outs() << "We just constructed this LLVM module:\n\n" << *M;
    outs().flush();
}