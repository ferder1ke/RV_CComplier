/* ************************************************************************
> File Name:     parse.c
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:30:20 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"

Obj* Locals;

static Obj* findVar(Token* Tok) {
    for(Obj* obj = Locals; obj; obj = obj->Next) {
        if((strlen(obj->Name) == Tok->Len) 
                && !strncmp(Tok->Pos, obj->Name, Tok->Len)){
            return obj;
        }
    }
    return NULL;
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

static Node* newVar(Obj* Var, Token* Tok) {
    Node* Nd = newNode(ND_VAR, Tok);
    Nd->Var = Var;
    return Nd;
}

static Obj* newLVar(char* Name, Type* Ty) {
    Obj* obj = calloc(1, sizeof(Obj));
    obj->Name = Name;
    obj->Next = Locals;
    obj->Ty = Ty;
    Locals = obj;
    return obj;
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
    RHS = newBinary(ND_MUL, RHS, newNum(8, Tok), Tok);
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
        RHS = newBinary(ND_MUL, RHS, newNum(8, Tok), Tok);
        addType(RHS);
        Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
        Nd->Ty = LHS->Ty;
        return Nd;
    }

    //ptr - ptr
    if(LHS->Ty->Base && RHS->Ty->Base) {
        
        Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
        Nd->Ty = TypeInt;
        return newBinary(ND_DIV, Nd, newNum(8, Tok), Tok);
    }
    errorTok(Tok, "invalid operands");
    return NULL;
}

static Type* typeSuffix(Token** Rest, Token* Tok, Type* ReturnTy) {
    if(equal(Tok, "(")) {
        *Rest = skip(Tok->Next, ")");
        return funcType(ReturnTy);
    }
    *Rest = Tok;
    return ReturnTy;
}

// program = functionDefinition*
// functionDefinition = declspec declarator "{" compoundStmt*
// declspec = "int"
// declarator = "*"* ident typeSuffix
// typeSuffix = ("(" ")")?

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
//unary = (+ | - | * | &) unary |  primary 
//primary = "(" expr ")" | num | ident func-args?
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
static Node* unary(Token** Rest, Token* Tok);
static Node* funcall(Token** Rest, Token* Tok);

static Node* compoundStmt(Token** Rest, Token* Tok) {
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
   
   Node* Nd = newNode(ND_BLOCK, Tok);
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
        
        Node* LHS = newVar(Var, Ty->Name);
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
        Node* Nd = newUnary(ND_RETURN, expr(&Tok, Tok->Next), Tok);
        *Rest = skip(Tok, ";");
        return Nd;
    }


    if(equal(Tok, "while")) {
        Node* Nd = newNode(ND_FOR, Tok);
        Tok = skip(Tok->Next, "(");
        Nd->Cond = expr(&Tok, Tok);
        Tok = skip(Tok, ")");
        Nd->Then = stmt(&Tok, Tok);
        *Rest = Tok;
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
        
        Nd->Then = stmt(&Tok, Tok);
        *Rest = Tok;
        return Nd;
    }
    
    if(equal(Tok, "if")) {
        Node* Nd = newNode(ND_IF, Tok);
        Tok = skip(Tok->Next, "(");
        Nd->Cond = expr(&Tok, Tok);
        Tok = skip(Tok, ")");
        Nd->Then = stmt(&Tok, Tok);
        if(equal(Tok, "else")) {
            Tok = skip(Tok, "else");
            Nd->Els = stmt(&Tok, Tok);
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

    return primary(Rest, Tok);
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
        return newVar(Var, Tok);
    }else if(Tok->Kind == TK_NUM) {
        Node* Nd = newNum(Tok->Val, Tok);
        *Rest = Tok->Next;
        return Nd;
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

static Function* function(Token** Rest, Token* Tok) {
    Type* Ty = declspec(&Tok, Tok);
    Ty = declarator(&Tok, Tok, Ty);

    Function* func = calloc(1, sizeof(Function));
    Locals = NULL;

    func->Name = genIdent(Ty->Name);
    Tok = skip(Tok, "{");
    func->Body = compoundStmt(Rest, Tok);
    func->Locals = Locals;
    return func;
}

Function* parse(Token *Tok) {
    Function Head = {};
    Function* CurFunc = &Head;
    while(Tok->Kind != TK_EOF) {
        CurFunc = CurFunc->Next = function(&Tok, Tok);
    }
    return Head.Next;
}
