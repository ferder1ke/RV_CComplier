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

static Obj* newLVar(char* Name) {
    Obj* obj = calloc(1, sizeof(Obj));
    obj->Name = Name;
    obj->Next = Locals;
    Locals = obj;
    return obj;
}

// program = "{" compoundStmt
// compoundStmt = stmt* "}";
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
//unary = (+ | -) unary |  primary 
//primary = "(" expr ")" | num | ident
//preorder

static Node* compoundStmt(Token** Rest, Token* Tok);
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

static Node* compoundStmt(Token** Rest, Token* Tok) {
   Node Head = {};
   Node* Cur = &Head;
   while(!equal(Tok, "}")) {
       Cur->Next = stmt(&Tok, Tok);
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
            Nd = newBinary(ND_ADD, Nd, mul(&Tok, Tok->Next), Tok);
            continue;
        }else if(equal(Tok, "-")) {
            Nd = newBinary(ND_SUB, Nd, mul(&Tok, Tok->Next), Tok);
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
    }
    return primary(Rest, Tok);
}

static Node* primary(Token** Rest, Token* Tok) {
    if(equal(Tok, "(")) {
        Node* Nd = expr(&Tok, Tok->Next);
        *Rest = skip(Tok, ")");
        return Nd;
    }else if(Tok->Kind == TK_IDENT) {
        Obj* Var = findVar(Tok);
        if(!Var){
            Var = newLVar(strndup(Tok->Pos, Tok->Len));
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

Function* parse(Token *Tok) {
    Tok = skip(Tok, "{");
    Function* prog = calloc(1, sizeof(Function));
    prog->Body = compoundStmt(&Tok, Tok);
    prog->Locals = Locals;
    return prog;
}
