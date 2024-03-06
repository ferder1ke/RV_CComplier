/* ************************************************************************
> File Name:     rvcc.h
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:18:03 PM CST
> Description:   
 ************************************************************************/
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
    TK_IDENT
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
    ND_VAR          // var
} NodeKind;

typedef struct Node Node;
struct Node{
    NodeKind Kind;
    int Val;
    Node* Next;
    Node* LHS;
    Node* RHS;
    char Name;
};

Node *parse(Token *Tok);
void codegen(Node *Nd);
