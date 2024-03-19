/* ************************************************************************
> File Name:     parse.c
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:30:20 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"

Obj* Locals;
Obj* Globals;

typedef struct Scope Scope;
typedef struct VarScope VarScope;
typedef struct TagScope TagScope;

struct TagScope {
    TagScope* Next;
    char* Name;
    Type* Ty;
};

static Node* structRef(Node* LHS, Token* Tok); 
static Type* structDecl(Token** Rest, Token* Tok); 
static Type* unionDecl(Token** Rest, Token* Tok);

struct Scope {
    Scope* Next;
    VarScope* Vars;
    TagScope* Tags;
};

struct VarScope {
    VarScope* Next;
    char* Name;
    Obj* Vars;

};

static Scope *Scp = &(Scope) {};

static void enterScope(void) {
    Scope* S = calloc(1, sizeof(Scope));
    S->Next = Scp;
    Scp = S;
}

static void leaveScope(void) {
    Scp = Scp->Next;
}

static Type* findTag(Token* Tok) {
    for(Scope* S = Scp; S; S = S->Next) 
        for(TagScope* S2 = S->Tags; S2; S2 = S2->Next)
            if(equal(Tok, S2->Name))
                return S2->Ty;
    return NULL;
}

static void* pushTagScope(Token* Tok, Type* Ty) {
    TagScope* S = calloc(1, sizeof(TagScope));
    S->Ty = Ty;
    S->Name = strndup(Tok->Pos, Tok->Len);
    S->Next = Scp->Tags;
    Scp->Tags = S; 
}

static Obj* findVar(Token* Tok) {
    for(Scope* S = Scp; S; S = S->Next) 
        for(VarScope* S2 = S->Vars; S2; S2 = S2->Next)
            if(equal(Tok, S2->Name))
                return S2->Vars;
    return NULL;
}

static VarScope* pushScope(char* Name, Obj* Var) {
    VarScope* S = calloc(1, sizeof(VarScope));
    S->Name = Name;
    S->Vars = Var;
    S->Next = Scp->Vars;
    Scp->Vars = S; 
    return S;
}

static long getNumber(Token* Tok) {
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

static Node* newNum(int64_t Val, Token* Tok) {
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
    pushScope(Name, Var);
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
    if(equal(Tok, "char")) {
        *Rest = Tok->Next;
        return TypeChar;
    }

    if(equal(Tok, "int")) {
        *Rest = Tok->Next;
        return TypeInt;
    }
    
    if(equal(Tok, "long")) {
        *Rest = Tok->Next;
        return TypeLong;
    }
    
    if(equal(Tok, "short")) {
        *Rest = Tok->Next;
        return TypeShort;
    }

    if(equal(Tok, "struct")) {
        return structDecl(Rest, Tok->Next);
    }
    
    if(equal(Tok, "union")) {
        return unionDecl(Rest, Tok->Next);
    }

    errorTok(Tok, "typename expected");
    return NULL;
}

Type* declarator(Token** Rest, Token* Tok, Type* Ty) {
    while(consume(&Tok, Tok, "*")) {
        Ty = pointerTo(Ty);
    }
    if(equal(Tok, "(")) {
        Type Dummy = {};
        Token* Start = Tok;
        declarator(&Tok, Tok->Next, &Dummy);
        Tok = skip(Tok, ")");
        Ty = typeSuffix(Rest, Tok, Ty);
        return declarator(&Tok, Start->Next, Ty);
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
        return newBinary(ND_DIV, Nd, newNum(LHS->Ty->Base->Size, Tok), Tok);
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

static bool isTypename(Token* Tok) {
    if(equal(Tok, "int") || equal(Tok, "long") || equal(Tok, "short") || equal(Tok, "char") || equal(Tok, "struct") || equal(Tok, "union"))
        return true;
    return false;
}

static char* newUniqueName(void) {
    static int Id = 0;
    return format(".L..%d", Id++);
}

Obj* newAnonGVar(Type* Ty) {
    return newGVar(newUniqueName(), Ty);
}

static Obj* newStringLiteral(char* Str, Type* Ty) {
    Obj* Var = newAnonGVar(Ty);
    Var->InitData = Str;
    return Var;
}

// program = (functionDefinition | globalVariable)*
// functionDefinition = declspec declarator "{" compoundStmt*
// declspec = "int" | "long" | "short" | "char" | "structDecl" | "unionDecl" 
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

// expr = assign (',' expr)?
// assign = equality ("=" assign)?

// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
//mul = unary ("*" unary | "/" unary)*
//unary = (+ | - | * | &) unary |  postfix
//postfix = primary ("[" expr "]" | "." indent | "->" indent)*

//primary = "(" "{" stmt+  "}" ")"
//          | "(" expr ")" 
//          | num 
//          | "sizeof" unary 
//          | ident func-args? 
//          | str

// structMembers = (declspec declarator (","  declarator)* ";")*
// structDecl = structUnionDecl
// unionDecl = structUnionDecl
// structUnionDecl = ident? ("{" structMembers)?

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
   enterScope();
   while(!equal(Tok, "}")) {
       if(isTypename(Tok)) {
           Cur->Next  = declaration(&Tok, Tok);
           Cur = Cur->Next;
       }else {
           Cur->Next = stmt(&Tok, Tok);
           Cur = Cur->Next;
       }
       addType(Cur);
   }
   leaveScope();
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
    Node* Nd = assign(&Tok, Tok); // Remember if trans Rest unless &Tok, the Token pointer val is not change!!! 
    if(equal(Tok, ","))
        return newBinary(ND_COMMA, Nd, expr(Rest, Tok->Next), Tok);
    *Rest = Tok;
    return Nd;
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
    while(true) {
        
        if(equal(Tok, "[")) {
            Token* Start = Tok;
            Node* Idx = expr(&Tok, Tok->Next);
            Tok = skip(Tok, "]");
            Nd = newUnary(ND_DEREF, newAdd(Nd, Idx, Start), Start);
            continue;
        }

        if(equal(Tok, ".")) {
            Nd = structRef(Nd, Tok->Next); 
            Tok = Tok->Next->Next;
            continue;
        }

        if(equal(Tok, "->")) {
            Nd = newUnary(ND_DEREF, Nd, Tok);
            Nd = structRef(Nd, Tok->Next); 
            Tok = Tok->Next->Next;
            continue;
        }
        *Rest = Tok;
        return Nd;
    }
}

static Node* primary(Token** Rest, Token* Tok) {
    if(equal(Tok, "(") && equal(Tok->Next, "{")) {
        Node* Nd = newNode(ND_STMT_EXPR, Tok);
        Nd->Body = compoundStmt(&Tok, Tok->Next->Next)->Body;
        *Rest = skip(Tok, ")");
        return Nd;
    }
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
    if(Tok->Kind == TK_STR) {
        Obj* Var = newStringLiteral(Tok->Str, Tok->Ty);
        *Rest = Tok->Next;
        return newVarNode(Var, Tok);
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

    Fn->IsDefinition = !consume(&Tok, Tok, ";");
    if(!Fn->IsDefinition)
        return Tok;

    Locals = NULL;
    enterScope(); 
    createParamLVars(Ty->Param);
    Fn->Param = Locals;

    Tok = skip(Tok, "{");
    Fn->Body = compoundStmt(&Tok, Tok);
    Fn->Locals = Locals;
    leaveScope();
    return Tok;
}

static Token* globalVariable(Token* Tok, Type* BaseTy) {
    bool First = true;
    while(!consume(&Tok, Tok, ";")) {
        if(!First)
            Tok = skip(Tok, ",");
        First = false;
        Type* Ty = declarator(&Tok, Tok, BaseTy);
        newGVar(genIdent(Ty->Name), Ty);
    }
    return Tok;
}

static bool isFunction(Token* Tok) {
    if(equal(Tok, ";"))
        return false;
    
    Type Dummy = {};
    Type* Ty = declarator(&Tok, Tok, &Dummy);
    return Ty->typeKind == TypeFunc;
}

static void structMembers(Token** Rest, Token* Tok, Type* Ty){
    Member Head = {};
    Member* Cur = &Head;

    while(!equal(Tok, "}")) {
        Type* BaseTy = declspec(&Tok, Tok);    
        int First = true;
        while(!consume(&Tok, Tok, ";")) {
            if(!First) {
                Tok = skip(Tok, ",");
            }
            First = false;
            Member* Mem = calloc(1, sizeof(Member));
            Mem->Ty = declarator(&Tok, Tok, BaseTy);
            Mem->Name = Mem->Ty->Name;
            Cur = Cur->Next = Mem;
        }
    }
    *Rest = Tok->Next;
    Ty->Mem = Head.Next;
}

static Type* structUnionDecl(Token** Rest, Token* Tok) { //struct declaration
    Token* Tag = NULL;
    if(Tok->Kind == TK_IDENT) {
        Tag = Tok;
        Tok = Tok->Next;
    }

    if(Tag && !equal(Tok, "{")) {
        Type* Ty = findTag(Tag);
        if(!Ty) {
            errorTok(Tag, "unknown struct Type");
        }
        *Rest = Tok;
        return Ty;
    }
    
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = TypeSTRUCT;
    structMembers(Rest, Tok->Next, Ty); //struct members Inits
    Ty->Align = 1; 
    
    if(Tag)
        pushTagScope(Tag, Ty);
    return Ty;
}

static Type* structDecl(Token** Rest, Token* Tok) { //struct declaration
    Type* Ty = structUnionDecl(Rest, Tok);
    Ty->typeKind = TypeSTRUCT;
    
    int Offset = 0;
    for(Member* mem = Ty->Mem; mem; mem = mem->Next) {
        Offset = alignTo(Offset, mem->Ty->Align);
        mem->Offset = Offset;
        Offset += mem->Ty->Size;
        if(Ty->Align < mem->Ty->Align) 
            Ty->Align = mem->Ty->Align;
    }
    Ty->Size = alignTo(Offset, Ty->Align);
    return Ty;
}

static Type* unionDecl(Token** Rest, Token* Tok) { //union declaration
    Type* Ty = structUnionDecl(Rest, Tok);
    Ty->typeKind = TypeUNION;

    for(Member* mem = Ty->Mem; mem; mem = mem->Next) {
        if(Ty->Align < mem->Ty->Align)
            Ty->Align = mem->Ty->Align;
        if(Ty->Size < mem->Ty->Size)
            Ty->Size = mem->Ty->Size;
    }

    Ty->Size = alignTo(Ty->Size, Ty->Align);
    return Ty;
}

static Member* getStructMember(Type* Ty, Token* Tok) {
    for(Member* mem = Ty->Mem; mem; mem = mem->Next) {
        if (mem->Name->Len == Tok->Len &&
        !strncmp(mem->Name->Pos, Tok->Pos, Tok->Len))
        return mem;
    }
    errorTok(Tok, "no such member");
    return NULL;
}

static Node* structRef(Node* LHS, Token* Tok) {
    addType(LHS);
    if(LHS->Ty->typeKind != TypeSTRUCT && LHS->Ty->typeKind != TypeUNION)
         errorTok(LHS->Tok, "not a struct");
    Node *Nd = newUnary(ND_MEMBER, LHS, Tok);
    Nd->Mem = getStructMember(LHS->Ty, Tok);
    return Nd;
}

Obj* parse(Token *Tok) {
    Globals = NULL;
    while(Tok->Kind != TK_EOF) {
        Type* BaseTy = declspec(&Tok, Tok);
        if(isFunction(Tok)){
            Tok = function(Tok, BaseTy);
            continue;
        }
        Tok = globalVariable(Tok, BaseTy);    
    }
    return Globals;
}
