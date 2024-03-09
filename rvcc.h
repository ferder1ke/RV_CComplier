/* ************************************************************************
> File Name:     rvcc.h
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:18:03 PM CST
> Description:   
 ************************************************************************/
#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>

typedef enum {
    TK_PUNCT,
    TK_NUM,
    TK_EOF,
    TK_IDENT,
    TK_KEYWORD
} TokenKind;

typedef struct Token Token;

struct Token{
    TokenKind Kind;
    Token* Next;
    int Val;
    int Len;
    char* Pos; 
};

void error(char* Fmt, ...); 
void errorAt(char *Loc, char *Fmt, ...);
void errorTok(Token *Tok, char *Fmt, ...);
bool equal(Token *Tok, char *Str);
Token *skip(Token *Tok, char *Str);
Token *tokenize(char *Input);

typedef struct Node Node;

typedef struct Obj Obj;
struct Obj {
  Obj *Next; 
  char *Name; 
  int Offset; 
};

typedef struct Function Function;
struct Function {
  Node *Body;    
  Obj *Locals;  
  int StackSize;
};

/*semantic analysis*/
typedef enum {
    ND_ADD,         // +
    ND_SUB,         // -
    ND_MUL,         // *
    ND_DIV,         // /
    ND_NUM,         // number
    ND_NEG,         // ~
    ND_EQ,          // ==
    ND_NE,          // !=
    ND_LT,          // <
    ND_LE,          // <=
    ND_EXPR_STMT,   // ;
    ND_ASSIGN,      // assign
    ND_VAR,         // var
    ND_RETURN,      // return
    ND_BLOCK,       // code block
    ND_IF,          // 'if' statement
    ND_FOR,         // 'for' statement
    ND_ADDR,        // &
    ND_DEREF        // *
} NodeKind;

typedef enum {
    TypeINT, //int
    TypePTR, //pointer
}TypeKind;


typedef struct Type Type;

struct Type {
    TypeKind typeKind;
    Type* Base;
};

extern Type* TypeInt;

typedef struct Node Node;
struct Node{
    NodeKind Kind;
    int Val;
    Node* Next;
    Node* LHS;
    Node* RHS;
    Obj*  Var;
    Node* Body;
    
    Node* Cond;
    Node* Els;
    Node* Then;
    Node* Init;
    Node* Inc;
    Token* Tok;
    Type* Ty;
};

bool isInteger(Type *TY);

void addType(Node* Nd);

Function *parse(Token *Tok);
void codegen(Function* Prog);
