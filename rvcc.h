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

static char *ArgReg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};

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

bool consume(Token** Rest, Token* Tok, char* Str);
void error(char* Fmt, ...); 
void errorAt(char *Loc, char *Fmt, ...);
void errorTok(Token *Tok, char *Fmt, ...);
bool equal(Token *Tok, char *Str);
Token *skip(Token *Tok, char *Str);
Token *tokenize(char *Input);

typedef enum {
    TypeINT,  //int
    TypePTR,  //pointer
    TypeFunc, //Function
    TypeARRAY //Arrary
}TypeKind;


typedef struct Type Type;

struct Type {
    TypeKind typeKind;
    int Size;
    
    Type* Base;
    Token* Name;
    
    int ArraryLen;
   
    //Function
    Type* ReturnTy;
    Type* Param;
    Type* Next;
};
typedef struct Node Node;

typedef struct Obj Obj;
struct Obj {
  // Varibles
  Obj *Next; 
  char *Name; 
  Type* Ty;
  bool IsLocal;

  int Offset; 
  //Function
  bool IsFunction;
  Obj* Param;

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
    ND_DEREF,        // *
    ND_FUNCALL      // Function calloc
} NodeKind;


extern Type* TypeInt;

Type* pointerTo(Type* Base);

typedef struct Node Node;
struct Node{
    NodeKind Kind;
    int Val;
    Node* Next;
    Node* LHS;
    Node* RHS;
    Obj*  Var;
    Node* Body;
    
    char* FuncName;
    Node* Args;

    Node* Cond;
    Node* Els;
    Node* Then;
    Node* Init;
    Node* Inc;
    Token* Tok;
    Type* Ty;
};

bool isInteger(Type *TY);

Type *copyType(Type *Ty);
void addType(Node* Nd);
Type* funcType(Type* ReturnTy);
Obj *parse(Token *Tok);
void codegen(Obj* Prog);
static Type* typeSuffix(Token** Rest, Token* Tok, Type* Ty); 
Type* arrayof(Type* Base, int Size);
