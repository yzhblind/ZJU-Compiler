#include "llvm_ir.hpp"

IR_builder::IR_builder() {
    InitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    Owner = make_unique<Module>("test", Context);
    M = Owner.get();
}
IR_builder::~IR_builder() {

}


void IR_builder::CodeGen(ASTRoot* root) {
    Function* Main = Function::Create(FunctionType::get(Type::getInt64Ty(Context), {}, false), Function::ExternalLinkage, "main", M);
    BasicBlock* MainB = BasicBlock::Create(Context, "Global", Main);
    IRBuilder<> builder(MainB);

    auto Cprint = [&](vector<Value*>args, bool new_line) {
        static Function* llvm_printf = nullptr;
        if (llvm_printf == nullptr) {
            //build printf
            vector<Type*> arg_types = { Type::getInt8PtrTy(Context) };
            FunctionType* func_type = FunctionType::get(Type::getInt32Ty(Context), arg_types, true);
            Function* func = Function::Create(func_type, Function::ExternalLinkage, "printf", M);
            func->setCallingConv(CallingConv::C);
            llvm_printf = func;
        }
        string format;
        vector<Value*> printf_args;
        printf_args.emplace_back(nullptr);
        for (auto arg : args) {
            auto type = arg->getType()->getTypeID();
            using T = Type::TypeID;
            if (type == T::IntegerTyID) {
                format += "%d";
                printf_args.emplace_back(arg);
            }
            else if (type == T::DoubleTyID) {
                format += "%lf";
                printf_args.emplace_back(arg);
            }
            // TODO string and etc
        }
        if (new_line) {
            format += "\n";
        }
        printf_args[0] = builder.CreateGlobalStringPtr(format, "printf_format");
        return builder.CreateCall(llvm_printf, printf_args, "call_printf");
    };

    map<string, Value*> Value_map;

    Value* con_0 = builder.getInt64(0);

    function<Value* (ASTConstValue*)> build_constant_value = [&](ASTConstValue* val) {
        auto type = val->value_type;
        using T = ASTConstValue::ConstType;
        if (type == T::INT) {
            Value* ret = builder.CreateAlloca(Type::getInt64Ty(Context));
            builder.CreateStore(builder.getInt64(val->int_value), ret);
            return ret;
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

    function<Value* (ASTConstValue*)> get_constant_value = [&](ASTConstValue* val) {
        auto type = val->value_type;
        using T = ASTConstValue::ConstType;
        if (type == T::INT) {
            Value* ret = builder.getInt64(val->int_value);
            return ret;
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
            auto V = Value_map[dynamic_cast<ASTVarAccessId*>(var)->id];

            return V;
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
                builder.CreateStore(con_0, ret);
                return ret;
            }
            else if (ret->id == "real") {
                Value* ret = builder.CreateAlloca(Type::getDoubleTy(Context));
                //TODO store一个0
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
                        return get_exp_value(dynamic_cast<ASTFactorExpr*>(expr)->expr);
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
                    return ret;
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
            auto type = expr->rop;
            using T = ASTExpr::ROP;
            Value* ret;
            //这里还没有区分整数和浮点数 TODO

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
            return ret;
        }
        return get_simple(expr->left);
    };

    function<void(const string&, ASTStmt*)> Stmt_Gen = [&](const string& pref, ASTStmt* stmt) {
        auto type = stmt->get_stmt_type();
        using T = ASTStmt::TypeKind;
        if (type == T::EMPTY) return;
        else if (type == T::ASSIGN) {
            auto it = dynamic_cast<ASTAssignStmt*>(stmt);
            Value* right = get_exp_value(it->right);
            Value* left = var_access(it->left);
            builder.CreateStore(right, left);
        }
        else if (type == T::PROCEDURE_CALL) {

        }
        else if (type == T::IF) {
            auto it = dynamic_cast<ASTIfStmt*>(stmt);

            auto cur = builder.GetInsertBlock();

            BasicBlock* True_Block = BasicBlock::Create(Context, pref + "true_block", Main);
            BasicBlock* False_Block = BasicBlock::Create(Context, pref + "false_block", Main);
            BasicBlock* Cont_Block = BasicBlock::Create(Context, pref + "cont_block", Main);
            builder.CreateCondBr(get_exp_value(it->cond), True_Block, False_Block);
            
            builder.SetInsertPoint(True_Block);
            Stmt_Gen(pref + "IF_True", it->true_block);
            builder.CreateBr(Cont_Block);//back

            builder.SetInsertPoint(False_Block);
            Stmt_Gen(pref + "IF_False", it->false_block);
            builder.CreateBr(Cont_Block);//back

            builder.SetInsertPoint(Cont_Block);
        }
        else if (type == T::REPEAT) {

        }
        else if (type == T::WHILE) {

        }
        else if (type == T::FOR) {

        }
    };

    {
        auto it = root->const_def;
        while (it) {
            Value_map[it->id] = build_constant_value(it->value);
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
        //这里构建函数 应该把上面的部分封装起来，返回一个Function*
        //TODO
    }

    int cnt = 0; //好像重名会自动加后缀 TODO
    {
        auto it = root->stmt;
        //程序主体部分
        while (it) {
            Stmt_Gen("stmt" + to_string(cnt += 1), it);
            it = it->next_stmt;
        }
    }
    //Cprint({ Value_map["OUT"] }, true);
    
    builder.CreateRet(builder.CreateLoad(Type::getInt64Ty(Context), Value_map["OUT"]));
    outs() << "We just constructed this LLVM module:\n\n" << *M;
    outs().flush();
    

    ExecutionEngine* EE = EngineBuilder(std::move(Owner)).create();

    std::vector<GenericValue> noargs;
    GenericValue gv = EE->runFunction(Main, noargs);

    // Import result of execution:
    outs() << "Result: " << gv.IntVal << "\n";
    delete EE;
    llvm_shutdown();
}