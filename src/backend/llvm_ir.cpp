#include "llvm_ir.hpp"

IR_builder::IR_builder() {
    InitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    Owner = make_unique<Module>("test", Context);
    M = Owner.get();
}
IR_builder::~IR_builder() {

}


string to_low(const string& x) {
    string tmp = x;
    transform(tmp.begin(), tmp.end(), tmp.begin(), ::tolower);
    return tmp;
}

void IR_builder::CodeGen(ASTRoot* root) {
    Function* Main = Function::Create(FunctionType::get(Type::getInt64Ty(Context), {}, false), Function::ExternalLinkage, "main", M);
    BasicBlock* MainB = BasicBlock::Create(Context, "Entry", Main);
    IRBuilder<> builder(MainB);

    function<Value* (ASTExpr*)> get_exp_value;

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
            format.push_back(' ');
            // TODO string and etc
        }
        format.pop_back();
        if (new_line) {
            format += "\n";
        }
        printf_args[0] = builder.CreateGlobalStringPtr(format, "printf_format");
        return builder.CreateCall(llvm_printf, printf_args, "call_printf");
    };

    map<string, Value*> Value_map; //Global map
    map<string, int64_t> Const_map; //only for int
    /*
        这里const没有考虑局部还是全局
    */
    map<string, Function*> Func_map;

    map<string, Value*>* local = &Value_map;

    Value* con_0 = builder.getInt64(0);

    function<int(ASTConstValue*)> get_const = [&](ASTConstValue* val) {
        auto type = val->value_type;
        using T = ASTConstValue::ConstType;
        if (type == T::INT) {
            return (int)(val->int_value);
        }
        else if (type == T::REAL) {
            
        }
        else if (type == T::STRING) {
            
        }
        else if (type == T::ID) {
            //???? 这里有BUG：没有区分local还是global
            cout << "ID! " << val->str << endl;
            return (int)Const_map[val->str];
        }
        else {
            //????
            assert(type == T::NIL);
        }
        return 0;
    };

    function<Value* (ASTConstValue*, string)> build_constant_value = [&](ASTConstValue* val, string name) {
        auto type = val->value_type;
        using T = ASTConstValue::ConstType;
        if (type == T::INT) {
            Value* ret = builder.CreateAlloca(Type::getInt64Ty(Context));
            builder.CreateStore(builder.getInt64(val->int_value), ret);
            Const_map[name] = val->int_value;
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
            auto idx = dynamic_cast<ASTVarAccessId*>(var)->id;
            auto it = local->find(idx);
            if (it != local->end()) {
                return it->second;
            }
            return Value_map.find(idx)->second;
        }
        else if (type == T::INDEX) {
            auto ret = dynamic_cast<ASTVarAccessIndex*>(var);
            auto tmp = ret->idx;
            Value* idx = get_exp_value(tmp[0]);

            string name = dynamic_cast<ASTVarAccessId*>(ret->arr)->id;
            Value* arr;
            auto it = local->find(name);
            if (it != local->end()) {
                arr = it->second;
            }
            arr = Value_map.find(name)->second;
            return builder.CreateGEP(arr, {con_0, idx});
        }
        else if (type == T::FIELD) {

        }
        else if (type == T::PTR) {

        }
    };

    function<Value* (ASTType*)> build_var = [&](ASTType* val) {
        auto type = val->get_type();
        using T = ASTType::TypeKind;
        Value* ret;
        if (type == T::ID) {
            auto ret = dynamic_cast<ASTTypeId*>(val);
            if (to_low(ret->id) == "integer") {
                Value* ret = builder.CreateAlloca(Type::getInt64Ty(Context));
                builder.CreateStore(con_0, ret);
                return ret;
            }
            else {
                cout << ret->id << endl;
                assert(0);
            }
            /*
            else if (ret->id == "real") {
                Value* ret = builder.CreateAlloca(Type::getDoubleTy(Context));
                //TODO store一个0
                return ret;
            }*/
        }
        else if (type == T::ARRAY) {
            auto ret = dynamic_cast<ASTTypeArray*>(val);
            assert(ret->element_type->get_type() == ASTType::TypeKind::ID);
            assert(to_low(dynamic_cast<ASTTypeId*>(ret->element_type)->id) == "integer");
            vector<ASTTypeSubrange*> tmp = ret->index;
            ASTTypeSubrange* O = tmp[0];
            int le = get_const(O->left);
            int ri = get_const(O->right);
            
            Value* vet = builder.CreateAlloca(ArrayType::get(Type::getInt64Ty(Context), ri - le + 1));
            return vet;
        }
    };

    get_exp_value = [&](ASTExpr* expr) {

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
                        auto ret = dynamic_cast<ASTFactorFunc*>(expr);
                        auto it = ret->para;
                        vector<Value*> args;
                        while (it) {
                            args.emplace_back(get_exp_value(it->expr));
                            it = it->next_actual_para;
                        }
                        //TODO 假设参数全是表达式了
                        Value* tmp = builder.CreateCall(Func_map[ret->id], args);
                        return tmp;
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

    function<void(const string&, ASTStmt*, Function*)> Stmt_Gen = [&](const string& pref, ASTStmt* stmt, Function* Fun) {
        auto type = stmt->get_stmt_type();
        using T = ASTStmt::TypeKind;

        if (type == T::EMPTY) return;
        else if (type == T::ASSIGN) {
            auto it = dynamic_cast<ASTAssignStmt*>(stmt);
            Value* right = get_exp_value(it->right);
            Value* left;
            if (it->left == nullptr)
                left = local->find("RESULT")->second;
            else 
                left = var_access(it->left);
            
            builder.CreateStore(right, left);
        }
        else if (type == T::PROCEDURE_CALL) {
            auto it = dynamic_cast<ASTProcStmt*>(stmt);
            auto type = it->get_proc_type();
            using T = ASTProcStmt::TypeKind;
            if (type == T::NORMAL) {
                assert(0);
            }
            else if (type == T::WRITE) {
                auto w_stmt = dynamic_cast<ASTWriteStmt*>(stmt);
                auto args = w_stmt->para->write_para;
                vector<Value*> arg;
                for (auto x : args) 
                    arg.emplace_back(get_exp_value(x->value));
                
                Cprint(arg, w_stmt->newline);
            }
            else {
                
            }
        }
        else if (type == T::IF) {
            auto it = dynamic_cast<ASTIfStmt*>(stmt);
            
            BasicBlock* True_Block = BasicBlock::Create(Context, pref + "true_block", Fun);
            BasicBlock* False_Block = BasicBlock::Create(Context, pref + "false_block", Fun);
            BasicBlock* Cont_Block = BasicBlock::Create(Context, pref + "cont_block", Fun);
            builder.CreateCondBr(get_exp_value(it->cond), True_Block, False_Block);
            
            builder.SetInsertPoint(True_Block);
            Stmt_Gen(pref + "IF_True", it->true_block, Fun);
            builder.CreateBr(Cont_Block);//back

            builder.SetInsertPoint(False_Block);
            Stmt_Gen(pref + "IF_False", it->false_block, Fun);
            builder.CreateBr(Cont_Block);//back

            builder.SetInsertPoint(Cont_Block);
        }
        else if (type == T::REPEAT) {
            auto it = dynamic_cast<ASTRepeatStmt*>(stmt);
            
            BasicBlock* Repeat_Body = BasicBlock::Create(Context, pref + "repeat_body", Fun);
            BasicBlock* Cont_Block = BasicBlock::Create(Context, pref + "cont_block", Fun);
            
            builder.CreateBr(Repeat_Body);
            builder.SetInsertPoint(Repeat_Body);

            Stmt_Gen(pref + "repeat", it->loop_body, Fun);
            
            builder.CreateCondBr(get_exp_value(it->cond), Cont_Block, Repeat_Body);
            builder.SetInsertPoint(Cont_Block);
        }
        else if (type == T::WHILE) {
            auto it = dynamic_cast<ASTWhileStmt*>(stmt);
            
            BasicBlock* While_Body = BasicBlock::Create(Context, pref + "while_body", Fun);
            BasicBlock* Cont_Block = BasicBlock::Create(Context, pref + "cont_block", Fun); //over
            BasicBlock* Cond_Block = BasicBlock::Create(Context, pref + "cond_block", Fun); //条件

            builder.CreateBr(Cond_Block);//back
            builder.SetInsertPoint(Cond_Block);
            
            builder.CreateCondBr(get_exp_value(it->cond), While_Body, Cont_Block);

            builder.SetInsertPoint(While_Body);
            Stmt_Gen(pref + "while", it->loop_body, Fun);
            builder.CreateBr(Cond_Block);
            
            builder.SetInsertPoint(Cont_Block);
        }
        else if (type == T::FOR) {

        }

        if (stmt->next_stmt)
            Stmt_Gen(pref, stmt->next_stmt, Fun);
    };

    function<vector<Type*>(map<string, Value*>&, ASTVarDecl*)> get_types = [&](map<string, Value*> &Value_map, ASTVarDecl* var_decl) {
        vector<Type*> ret;
        while (var_decl) {
            for (string str : var_decl->id_list) {
                ret.emplace_back(Type::getInt64Ty(Context));
            }
            var_decl = var_decl->next_var_decl;
        }
        return ret;
    };

    function<void(map<string, Value*>&, ASTVarDecl*)> build_map = [&](map<string, Value*> &Value_map, ASTVarDecl* var_decl) {
        while (var_decl) {
            for (string str : var_decl->id_list) {
                cout << "BUILD:" << str << endl;
                Value_map[str] = build_var(var_decl->var_type);
            }
            var_decl = var_decl->next_var_decl;
        }
    };
    
    function<void(map<string, Value*>&, ASTConstDef*)> build_const = [&](map<string, Value*>& Value_map, ASTConstDef* const_def) {
        while (const_def) {
            Value_map[const_def->id] = build_constant_value(const_def->value, const_def->id);
            const_def = const_def->next_const_def;
        }
    };

    function<void(ASTProcFuncDecl*)> Build_Func = [&](ASTProcFuncDecl* decl) {
        cout << "BEGIN FUNC" << endl;

        Function* Fun;
        auto type = decl->get_type();
        if (type == ASTProcFuncDecl::TypeKind::PROCEDURE) {
            
        }
        else {
            ASTFuncDecl* func_decl = dynamic_cast<ASTFuncDecl*>(decl);
            //先获取参数类型
            auto para = func_decl->para;
            
            map<string, Value*> Value_map_local;
            vector<Type*> func_type;
            auto o_para = para;
            while (para) {
                using T = ASTParameter::TypeKind;
                if (para->get_type() == T::VARIABLE) {
                    auto tmp = get_types(Value_map_local, para->var_decl);
                    for (auto p : tmp) func_type.emplace_back(p);//先假设全是不传引用的int了
                }
                else if (para->get_type() == T::REF_VARIABLE) {
                    //TODO
                }
                else {
                    //TODO
                }
                para = para->next_para;
            }
            assert(to_low(func_decl->ret_type_id) == "integer");
            
            Fun = Function::Create(FunctionType::get(Type::getInt64Ty(Context), func_type, false), Function::ExternalLinkage, decl->id, M);
            cout << "map!" << decl->id << endl;
            Func_map[decl->id] = Fun;
            BasicBlock* MainB = BasicBlock::Create(Context, decl->id + "Entry", Fun);
            builder.SetInsertPoint(MainB);
        
            local = &Value_map_local;
            para = o_para;

            int cnt = 0;
            Function::arg_iterator args_it = Fun->arg_begin();
            
            while (para) {
                using T = ASTParameter::TypeKind;
                if (para->get_type() == T::VARIABLE) {
                    while (para->var_decl) {
                        for (string str : para->var_decl->id_list) {
                            Value* ret = args_it++;
                            Value* pointer = builder.CreateAlloca(Type::getInt64Ty(Context));         
                            builder.CreateStore(ret, pointer);
                            Value_map_local[str] = pointer;
                        }
                        para->var_decl = para->var_decl->next_var_decl;
                    }
                }
                else if (para->get_type() == T::REF_VARIABLE) {
                    //TODO
                }
                else {
                    //TODO
                }
                para = para->next_para;
            }

            auto root = decl->block;
            build_const(Value_map_local, root->const_def);
            //type_def TODO
            build_map(Value_map_local, root->var_decl);
            
            Value* ret = builder.CreateAlloca(Type::getInt64Ty(Context));
            builder.CreateStore(con_0, ret);
            Value_map_local["RESULT"] = ret;

            ASTStmt* stmt = root->stmt;
            if (stmt) Stmt_Gen(decl->id + "_stmt", stmt, Fun);

            function<Value* (string)> load_var = [&](string str) {
                return builder.CreateLoad(Type::getInt64Ty(Context), Value_map_local[str]);
            };
            builder.CreateRet(load_var("RESULT"));
        }
        local = &Value_map;
    };
    
    function<void()> Main_builder = [&]() {
        build_const(Value_map, root->const_def);
        //type_def TODO
        build_map(Value_map, root->var_decl);

        ASTProcFuncDecl* func = root->proc_func_decl;
        while (func) {
            Build_Func(func);
            func = func->next_proc_func_decl;
        }
        
        builder.SetInsertPoint(MainB);

        ASTStmt* stmt = root->stmt;
        if (stmt) Stmt_Gen("main_stmt", stmt, Main);
    
        function<Value* (string)> load_var = [&](string str) {
            return builder.CreateLoad(Type::getInt64Ty(Context), Value_map[str]);
        };

        builder.CreateRet(builder.getInt64(0));
    };
    
    Main_builder();

    outs() << "We just constructed this LLVM module:\n\n" << *M;
    outs().flush();

    ExecutionEngine* EE = EngineBuilder(std::move(Owner)).create(); //JIT

    std::vector<GenericValue> noargs;
    GenericValue gv = EE->runFunction(Main, noargs);

    // Import result of execution:
    outs() << "Return value: " << gv.IntVal << "\n";
    delete EE;
    llvm_shutdown();
}
/*
复杂度
    构造某个定复杂度的算法 
    规约
DP

后缀树
    广义

matching
*/