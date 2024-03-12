/* ************************************************************************
> File Name:     parse.c
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:30:20 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"

Obj* Locals;
Obj* Globals;

static Obj* findVar(Token* Tok) {
    for(Obj* obj = Locals; obj; obj = obj->Next) {
        if((strlen(obj->Name) == Tok->Len) 
                && !strncmp(Tok->Pos, obj->Name, Tok->Len)){
            return obj;
        }
    }
    return NULL;
}

static int getNumber(Token* Tok) {
    if(Tok->Kind != TK_NUM)
        errorTok(Tok, "expected number");
    return Tok->Val;    
}

static Node* newNode(NodeKind Kind,Token* Tok) {
    Node* Nd = calloc(1, sizeof(Node));
    Nd->Kind = Kind;
    Nd->Tok = Tok;
    return Nd;


}

static Node* newUnary(NodeKind Kind, Node* Expr, Token* Tok) {
    Node* Nd = newNode(Kind, Tok);
    Nd->LHS = Expr;
    return Nd;
}

static Node* newBinary(NodeKind Kind, Node* LHS, Node* RHS, Token* Tok) {
    Node* Nd = newNode(Kind, Tok);
    Nd->LHS = LHS;
    Nd->RHS = RHS;
    return Nd;
}

static Node* newNum(int Val, Token* Tok) {
    Node* Nd = newNode(ND_NUM, Tok);
    Nd->Val = Val;
    return Nd;
}

static Node* newVarNode(Obj* Var, Token* Tok) {
    Node* Nd = newNode(ND_VAR, Tok);
    Nd->Var = Var;
    return Nd;
}

static Obj* newVar(char* Name, Type* Ty) {
    Obj* Var = calloc(1, sizeof(Obj));
    Var->Name = Name;
    Var->Ty = Ty;
    return Var;
}
static Obj* newLVar(char* Name, Type* Ty) {
    Obj* Var = newVar(Name, Ty);
    Var->IsLocal = true;
    Var->Next = Locals;
    Locals = Var;
    return Var;
}

static Obj* newGVar(char* Name, Type* Ty) {
    Obj* Var = newVar(Name, Ty);
    Var->Next = Globals;
    Globals = Var;
    return Var;
}

Type* declspec(Token** Rest, Token* Tok) {
    *Rest = skip(Tok, "int");
    return TypeInt;
}

Type* declarator(Token** Rest, Token* Tok, Type* Ty) {
    while(consume(&Tok, Tok, "*")) {
        Ty = pointerTo(Ty);
    }
    if(Tok->Kind != TK_IDENT){
        errorTok(Tok, "expected a varibles Name");
    }
    Ty = typeSuffix(Rest, Tok->Next, Ty);
    Ty->Name = Tok;
    return Ty;
}

static char* genIdent(Token* Tok) {
    if(Tok->Kind != TK_IDENT)
        errorTok(Tok, "expected identifier");
    return strndup(Tok->Pos, Tok->Len);
}

static Node* newAdd(Node* LHS, Node* RHS, Token* Tok) {
    addType(LHS);
    addType(RHS);

    //num + num
    if(isInteger(LHS->Ty) && isInteger(RHS->Ty)) {
        return newBinary(ND_ADD, LHS, RHS, Tok);
    }

    if (LHS->Ty->Base && RHS->Ty->Base)
        errorTok(Tok, "invalid operands");
    
    //num + ptr
    if(!LHS->Ty->Base && RHS->Ty->Base) {
        Node* tmp;
        tmp = RHS;
        RHS = LHS;
        LHS = tmp;
    }

    //ptr + num
    RHS = newBinary(ND_MUL, RHS, newNum(LHS->Ty->Base->Size, Tok), Tok);
    return newBinary(ND_ADD, LHS, RHS, Tok);
}

static Node* newSub(Node* LHS, Node* RHS, Token* Tok) {
    addType(LHS);
    addType(RHS);

    //num - num
    if(isInteger(LHS->Ty) && isInteger(RHS->Ty)) {
        return newBinary(ND_SUB, LHS, RHS, Tok);
    }

    //ptr -  num
    if(LHS->Ty->Base && isInteger(RHS->Ty)) {
        RHS = newBinary(ND_MUL, RHS, newNum(LHS->Ty->Base->Size, Tok), Tok);
        addType(RHS);
        Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
        Nd->Ty = LHS->Ty;
        return Nd;
    }

    //ptr - ptr
    if(LHS->Ty->Base && RHS->Ty->Base) {
        
        Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
        Nd->Ty = TypeInt;
        return newBinary(ND_DIV, RHS, newNum(LHS->Ty->Base->Size, Tok), Tok);
    }
    errorTok(Tok, "invalid operands");
    return NULL;
}

static void createParamLVars(Type* Param) {
    if(Param) {
        createParamLVars(Param->Next);
        newLVar(genIdent(Param->Name), Param);
    }
}

static Type* funcParams (Token** Rest, Token* Tok, Type* Ty) {
   Type Head = {};
   Type* Cur = &Head;
   while(!equal(Tok, ")")) {
       if(Cur != &Head)
           Tok = skip(Tok, ",");
       Type* BaseTy = declspec(&Tok, Tok);
       Type* DeclarTy = declarator(&Tok, Tok, BaseTy);
       Cur->Next = copyType(DeclarTy);
       Cur = Cur->Next;
   }
   
   Ty = funcType(Ty);
   Ty->Param = Head.Next;
   *Rest = Tok->Next;
   return Ty;
}

static Type* typeSuffix(Token** Rest, Token* Tok, Type* Ty) {
    if(equal(Tok, "(")) {
        return funcParams(Rest, Tok->Next, Ty);
    }

    if(equal(Tok, "[")) {
        int Sz = getNumber(Tok->Next);
        Tok  = skip(Tok->Next->Next, "]");
        Ty = typeSuffix(Rest, Tok, Ty);
        return arrayof(Ty, Sz);
    }
    *Rest = Tok;
    return Ty;
}

// program = (functionDefinition | globalVariable)*
// functionDefinition = declspec declarator "{" compoundStmt*
// declspec = "int"
// declarator = "*"* ident typeSuffix
// typeSuffix = ("(" funcParams? ")")?
// funcParams = param ("," param)*
// param = declspec declarator


// program = "{" compoundStmt
// compoundStmt = (declaration | stmt)* "}"
// declaration =
//    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"

// stmt = "return" expr ";" 
//       | "{"compoundStmt 
//       | exprStmt
//       | "if" "(" expr ")" stmt ("else" stmt)?
//       | "for" "(" exprStmt expr? ";" expr? ")" stmt  
//       | "while" "(" expr ")" stmt  
// exprStmt = expr? ";"

// expr = assign
// assign = equality ("=" assign)?

// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
//mul = unary ("*" unary | "/" unary)*
//unary = (+ | - | * | &) unary |  postfix
//postfix = primary ("[" expr "]")*
//primary = "(" expr ")" | num | "sizeof" unary | ident func-args?
//funcall = indent "("(assign (, assign)?)?")"
//preorder

static Node* compoundStmt(Token** Rest, Token* Tok);
static Node* declaration(Token** Rest, Token* Tok);
static Node* stmt(Token** Rest, Token* Tok);
static Node* exprStmt(Token** Rest, Token* Tok);
static Node* expr(Token** Rest, Token* Tok);
static Node* assign(Token** Rest, Token* Tok);
static Node* equality(Token** Rest, Token* Tok);
static Node* relational(Token** Rest, Token* Tok);
static Node* add(Token** Rest, Token* Tok);
static Node* mul(Token** Rest, Token* Tok);
static Node* primary(Token** Rest, Token* Tok);
static Node* postfix(Token** Rest, Token* Tok);
static Node* unary(Token** Rest, Token* Tok);
static Node* funcall(Token** Rest, Token* Tok);

static Node* compoundStmt(Token** Rest, Token* Tok) {
   Node* Nd = newNode(ND_BLOCK, Tok);
   Node Head = {};
   Node* Cur = &Head;
   while(!equal(Tok, "}")) {
       if(equal(Tok, "int")) {
           Cur->Next  = declaration(&Tok, Tok);
           Cur = Cur->Next;
       }else {
           Cur->Next = stmt(&Tok, Tok);
           Cur = Cur->Next;
       }
       addType(Cur);
   }
   
   Nd->Body = Head.Next;
   *Rest = Tok->Next;
   return Nd;
}

static Node* declaration(Token** Rest, Token* Tok) {
    Type* Basety = declspec(&Tok, Tok);
    
    Node Head = {};
    Node* Cur = &Head;
    
    int I = 0;

    while(!equal(Tok, ";")) {
        if(I++ > 0) {
            Tok = skip(Tok, ",");
        }
        
        Type* Ty = declarator(&Tok, Tok, Basety);
        Obj* Var = newLVar(genIdent(Ty->Name), Ty);//regist varibles

        if(!equal(Tok, "="))
            continue;
        
        Node* LHS = newVarNode(Var, Ty->Name);
        Node* RHS = assign(&Tok, Tok->Next);
        Node* Nd = newBinary(ND_ASSIGN, LHS, RHS, Tok);
        
        Cur->Next = newUnary(ND_EXPR_STMT, Nd, Tok);
        Cur = Cur->Next;
    }
    Node* Nd = newNode(ND_BLOCK, Tok);
    Nd->Body = Head.Next;
    *Rest = Tok->Next;
    return Nd;
}

static Node* stmt(Token** Rest, Token* Tok){
    if(equal(Tok, "return")) {
        Node* Nd = newNode(ND_RETURN, Tok);
        Nd->LHS = expr(&Tok, Tok->Next);
        *Rest = skip(Tok, ";");
        return Nd;
    }


    if(equal(Tok, "while")) {
        Node* Nd = newNode(ND_FOR, Tok);
        Tok = skip(Tok->Next, "(");
        Nd->Cond = expr(&Tok, Tok);
        Tok = skip(Tok, ")");
        Nd->Then = stmt(Rest, Tok);
        return Nd;
    }

    if(equal(Tok, "for")) {
        Node* Nd = newNode(ND_FOR, Tok);
        Tok = skip(Tok->Next, "(");
       
        Nd->Init = exprStmt(&Tok, Tok);
        
        if(!equal(Tok, ";"))
            Nd->Cond = expr(&Tok, Tok);
        Tok = skip(Tok, ";");

        if(!equal(Tok, ")"))
            Nd->Inc = expr(&Tok, Tok);
        Tok = skip(Tok, ")");
        
        Nd->Then = stmt(Rest, Tok);
        return Nd;
    }
    
    if(equal(Tok, "if")) {
        Node* Nd = newNode(ND_IF, Tok);
        Tok = skip(Tok->Next, "(");
        Nd->Cond = expr(&Tok, Tok);
        Tok = skip(Tok, ")");
        Nd->Then = stmt(&Tok, Tok);
        if(equal(Tok, "else")) {
            Nd->Els = stmt(&Tok, Tok->Next);
        }
        *Rest = Tok;
        return Nd;
    }

    if(equal(Tok, "{")) {
        return compoundStmt(Rest, Tok->Next);
    }
    Node* Nd = exprStmt(Rest, Tok);
    return Nd;
}

static Node* exprStmt(Token** Rest, Token* Tok) {
   //empty statment
    if(equal(Tok, ";")) {
        *Rest = Tok->Next;
        return newNode(ND_BLOCK, Tok);
    }
    Node* Nd = newUnary(ND_EXPR_STMT, expr(&Tok, Tok), Tok);
   *Rest = skip(Tok, ";");
   return Nd;
}

static Node* expr(Token** Rest, Token* Tok) {
    return assign(Rest, Tok);
}

static Node* assign(Token** Rest, Token* Tok) {
    Node* Nd = equality(&Tok, Tok);
    if(equal(Tok, "=")) {
        Nd = newBinary(ND_ASSIGN, Nd, assign(&Tok, Tok->Next), Tok);    
    }
    *Rest = Tok;
    return Nd;
}

static Node* equality(Token** Rest, Token* Tok) {
    Node* Nd = relational(&Tok, Tok);
    while(1) {
        if(equal(Tok, "==")) {
            Nd = newBinary(ND_EQ, Nd, relational(&Tok, Tok->Next), Tok);
            continue;
        }else if(equal(Tok, "!=")) {
            Nd = newBinary(ND_NE, Nd, relational(&Tok, Tok->Next), Tok);
            continue;
        }
        *Rest = Tok;
        return Nd;
    }
}

static Node* relational(Token** Rest, Token* Tok) {
    Node* Nd = add(&Tok, Tok);
    while(1) {
        if(equal(Tok, "<")) {
            Nd = newBinary(ND_LT, Nd, add(&Tok, Tok->Next), Tok);
            continue;
        }else if(equal(Tok, ">")) {
            Nd = newBinary(ND_LT, add(&Tok, Tok->Next), Nd, Tok);
            continue;
        }else if(equal(Tok, ">=")) {
            Nd = newBinary(ND_LE, add(&Tok, Tok->Next), Nd, Tok);
            continue;
        }else if(equal(Tok, "<=")) {
            Nd = newBinary(ND_LE, Nd, add(&Tok, Tok->Next), Tok);
            continue;
        }
        *Rest = Tok;
        return Nd;
    }
}

static Node* add(Token** Rest, Token* Tok) {
    Node* Nd = mul(&Tok, Tok);
    while(1) {
        if(equal(Tok, "+")) {
            Nd = newAdd(Nd, mul(&Tok, Tok->Next), Tok);
            continue;
        }else if(equal(Tok, "-")) {
            Nd = newSub(Nd, mul(&Tok, Tok->Next), Tok);
            continue;
        }
        *Rest = Tok;
        return Nd;
    }
}

static Node* mul(Token** Rest, Token* Tok) {
    Node* Nd = unary(&Tok, Tok);
    while(1) {
        if(equal(Tok, "*")) {
            Nd = newBinary(ND_MUL, Nd, unary(&Tok, Tok->Next), Tok);
            continue;
        }else if(equal(Tok, "/")) {
            Nd = newBinary(ND_DIV, Nd, unary(&Tok, Tok->Next), Tok);
            continue;
        }
        *Rest = Tok;
        return Nd;
    }
}

static Node* unary(Token** Rest, Token* Tok) {
    if(equal(Tok, "+")) {
       return unary(Rest, Tok->Next);    
    }else if(equal(Tok, "-")) {
       return newUnary(ND_NEG, unary(Rest, Tok->Next), Tok);
    }else if(equal(Tok, "&")) {
       return newUnary(ND_ADDR, unary(Rest, Tok->Next), Tok);
    }else if(equal(Tok, "*")) {
       return newUnary(ND_DEREF, unary(Rest, Tok->Next), Tok);
    }

    return postfix(Rest, Tok);
}


static Node* postfix(Token** Rest, Token* Tok) {
    Node* Nd =  primary(&Tok, Tok);
    while(equal(Tok, "[")) {
        Token* Start = Tok;
        Node* Idx = expr(&Tok, Tok->Next);
        Tok = skip(Tok, "]");
        Nd = newUnary(ND_DEREF, newAdd(Nd, Idx, Start), Start);
    }
    *Rest = Tok;
    return Nd;
}

static Node* primary(Token** Rest, Token* Tok) {
    if(equal(Tok, "(")) {
        Node* Nd = expr(&Tok, Tok->Next);
        *Rest = skip(Tok, ")");
        return Nd;
    }else if(Tok->Kind == TK_IDENT) {
        if(equal(Tok->Next, "(")) {//args
            return funcall(Rest, Tok);
        }
        Obj* Var = findVar(Tok);
        if(!Var){
            errorTok(Tok, "undefined varibles");
        }
        *Rest = Tok->Next;
        return newVarNode(Var, Tok);
    }else if(Tok->Kind == TK_NUM) {
        Node* Nd = newNum(Tok->Val, Tok);
        *Rest = Tok->Next;
        return Nd;
    }

    if(equal(Tok, "sizeof")) {
        Node* Nd = unary(Rest, Tok->Next);
        addType(Nd);
        return newNum(Nd->Ty->Size, Tok);
    }
    errorTok(Tok, "unexpected an expression");
    return NULL;
}

static Node* funcall(Token** Rest, Token* Tok) {
    Token* Start = Tok;
    Node Head = {};
    Node* Cur = &Head;

    Tok = Tok->Next->Next;
    while(!equal(Tok, ")")) {
        if(Cur != &Head) {
            Tok = skip(Tok, ",");
        }
        Cur->Next = assign(&Tok, Tok);
        Cur = Cur->Next;
    }
    Node* Nd = newNode(ND_FUNCALL, Start);
    Nd->FuncName = strndup(Start->Pos, Start->Len);
    Nd->Args = Head.Next;
    *Rest = skip(Tok, ")");
    return Nd;
}

static Token* function(Token* Tok, Type* BaseTy) {
    Type* Ty = declarator(&Tok, Tok, BaseTy);

    Obj* Fn = newGVar(genIdent(Ty->Name), Ty);
    Fn->IsFunction = true;
    Locals = NULL;
    
    createParamLVars(Ty->Param);
    Fn->Param = Locals;

    Tok = skip(Tok, "{");
    Fn->Body = compoundStmt(&Tok, Tok);
    Fn->Locals = Locals;
    return Tok;
}

Obj* parse(Token *Tok) {
    Globals = NULL;
    while(Tok->Kind != TK_EOF) {
        Type* BaseTy = declspec(&Tok, Tok);
        Tok = function(Tok, BaseTy);
    }
    return Globals;
}
