/* ************************************************************************
> File Name:     parse.c
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:30:20 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"

static Node* newNode(NodeKind Kind) {
    Node* Nd = calloc(1, sizeof(Node));
    Nd->Kind = Kind;
    return Nd;
}

static Node* newUnary(NodeKind Kind, Node* Expr) {
    Node* Nd = newNode(Kind);
    Nd->LHS = Expr;
    return Nd;
}

static Node* newBinary(NodeKind Kind, Node* LHS, Node* RHS) {
    Node* Nd = newNode(Kind);
    Nd->LHS = LHS;
    Nd->RHS = RHS;
    return Nd;
}

static Node* newNum(int Val) {
    Node* Nd = newNode(ND_NUM);
    Nd->Val = Val;
    return Nd;
}

// expr = equality
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
//mul = unary ("*" unary | "/" unary)*
//unary = "(" expr ")" | num*
//preorder
static Node* expr(Token** Rest, Token* Tok);
static Node* equality(Token** Rest, Token* Tok);
static Node* relational(Token** Rest, Token* Tok);
static Node* add(Token** Rest, Token* Tok);
static Node* mul(Token** Rest, Token* Tok);
static Node* primary(Token** Rest, Token* Tok);
static Node* unary(Token** Rest, Token* Tok);


static Node* expr(Token** Rest, Token* Tok) {
    return equality(Rest, Tok);
}


static Node* equality(Token** Rest, Token* Tok) {
    Node* Nd = relational(&Tok, Tok);
    while(1) {
        if(equal(Tok, "==")) {
            Nd = newBinary(ND_EQ, Nd, relational(&Tok, Tok->Next));
            continue;
        }else if(equal(Tok, "!=")) {
            Nd = newBinary(ND_NE, Nd, relational(&Tok, Tok->Next));
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
            Nd = newBinary(ND_LT, Nd, add(&Tok, Tok->Next));
            continue;
        }else if(equal(Tok, ">")) {
            Nd = newBinary(ND_LT, add(&Tok, Tok->Next), Nd);
            continue;
        }else if(equal(Tok, ">=")) {
            Nd = newBinary(ND_LE, add(&Tok, Tok->Next), Nd);
            continue;
        }else if(equal(Tok, "<=")) {
            Nd = newBinary(ND_LE, Nd, add(&Tok, Tok->Next));
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
            Nd = newBinary(ND_ADD, Nd, mul(&Tok, Tok->Next));
            continue;
        }else if(equal(Tok, "-")) {
            Nd = newBinary(ND_SUB, Nd, mul(&Tok, Tok->Next));
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
            Nd = newBinary(ND_MUL, Nd, unary(&Tok, Tok->Next));
            continue;
        }else if(equal(Tok, "/")) {
            Nd = newBinary(ND_DIV, Nd, unary(&Tok, Tok->Next));
            continue;
        }
        *Rest = Tok;
        return Nd;
    }
}

//unary = (+ | -) unary | expr
static Node* unary(Token** Rest, Token* Tok) {
    if(equal(Tok, "+")) {
       return unary(Rest, Tok->Next);    
    }else if(equal(Tok, "-")) {
       return newUnary(ND_NEG, unary(Rest, Tok->Next));
    }
    return primary(Rest, Tok);
}

static Node* primary(Token** Rest, Token* Tok) {
    if(equal(Tok, "(")) {
        Node* Nd = expr(&Tok, Tok->Next);
        *Rest = skip(Tok, ")");
        return Nd;
    }else if(Tok->Kind == TK_NUM) {
        Node* Nd = newNum(Tok->Val);
        *Rest = Tok->Next;
        return Nd;
    }
    errorTok(Tok, "unexpected an expression");
    return NULL;
}

Node *parse(Token *Tok) {
    Node *Nd = expr(&Tok, Tok);
    if (Tok->Kind != TK_EOF)
    errorTok(Tok, "extra token");
    return Nd;
}
