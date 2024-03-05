/* ************************************************************************
> File Name:     main.c
> Author:        ferdi
> 邮箱:          22s121106@stu.hit.edu.cn
> Created Time:  Wed 31 Jan 2024 03:11:53 PM CST
> Description:   
 ************************************************************************/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
/*Lexical analysis*/
static char* currentInput;
typedef enum {
    TK_PUNCT,
    TK_NUM,
    TK_EOF
} TokenKind;

typedef struct Token Token;

struct Token{
    TokenKind Kind;
    Token* Next;
    int Val;
    int Len;
    char* Pos; 
};

static void error(char* Fmt, ...) {
    va_list VA;
    va_start(VA, Fmt);
    vfprintf(stderr, Fmt, VA);
    fprintf(stderr, "\n");
    va_end(VA);
    exit(1);//Exception
}

static void __verrorAt(char* Loc, char* Fmt, va_list VA) {
    fprintf(stderr, "%s\n", currentInput);

    int Pos = Loc - currentInput;
    fprintf(stderr, "%*s", Pos, " ");
    fprintf(stderr, "^ ");
    vfprintf(stderr, Fmt, VA);
    fprintf(stderr, "\n");
    va_end(VA);
}

static void errorAt(char* Loc, char* Fmt, ...) {
    va_list VA;
    va_start(VA, Fmt);
    __verrorAt(Loc, Fmt, VA);
    exit(1);
}
static void errorTok(Token* Tok, char* Fmt, ...) {
    va_list VA;
    va_start(VA, Fmt);
    __verrorAt(Tok->Pos, Fmt, VA);
    exit(1);
}

static bool equal(Token* Tok, char* str){
    return memcmp(Tok->Pos, str, Tok->Len) == 0 && str[Tok->Len] == '\0';
}

static Token* skip(Token* Tok, char* str) {
    if(!equal(Tok, str))
        errorTok(Tok, "expect '%s'", str);
    return Tok->Next;
} 

static int getNum(Token* Tok) {
    if(Tok->Kind != TK_NUM)
        errorTok(Tok, "expect number!");
    return Tok->Val;
}

static Token* genToken(TokenKind Kind, char* Start, char* End) {
    Token* Tok = calloc(1, sizeof(Token));
    Tok->Kind = Kind;
    Tok->Pos = Start;
    Tok->Len = End - Start;
    return Tok;
}

static Token* tokenize() {
   Token Head = {};
   Token* cur = &Head;
   char*  P = currentInput;
   while(*P != '\0') {
       if(isspace(*P)) {
            ++P;
            continue;
       }

       if(isdigit(*P)) {
            cur->Next = genToken(TK_NUM, P, P);
            cur = cur->Next;
            const char* oldPtr = P;
            cur->Val = strtol(P, &P, 10);
            cur->Len = P - oldPtr;
            continue;
       }

       if(ispunct(*P)) {
            cur->Next = genToken(TK_PUNCT, P, P + 1);
            cur = cur->Next;
            ++P;
            continue;
       }
       errorAt(P, "invalid Token");
       
       error("unexpected character %s", P);
   }
   
   cur->Next = genToken(TK_EOF, P, P);
   return Head.Next;
}


/*semantic analysis*/
typedef enum {
    ND_ADD,// +
    ND_SUB,// -
    ND_MUL,// *
    ND_DIV,// /
    ND_NUM,// number
    ND_NEG,// ~
} NodeKind;

typedef struct Node Node;
struct Node{
    NodeKind Kind;
    int Val;
    Node* LHS;
    Node* RHS;
};

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

//expr = mul ("+" mul | "-" mul)
//mul = unary ("*" unary | "/" unary)
//unary = "(" expr ")" | num
//preorder
static Node* expr(Token** Rest, Token* Tok);
static Node* mul(Token** Rest, Token* Tok);
static Node* primary(Token** Rest, Token* Tok);
static Node* unary(Token** Rest, Token* Tok);

static Node* expr(Token** Rest, Token* Tok) {
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

/*Code gen*/
static int Depth;
static void push(void) {
  printf("  addi sp, sp, -8\n");
  printf("  sd a0, 0(sp)\n");
  Depth++;
}

static void pop(char *Reg) {
  printf("  ld %s, 0(sp)\n", Reg);
  printf("  addi sp, sp, 8\n");
  Depth--;
}

static void genExpr(Node* AST) {
    switch(AST->Kind) {
        case ND_NUM:
            printf("  li a0, %d\n", AST->Val);
            return;
        case ND_NEG:
             genExpr(AST->LHS);
             printf("  neg a0, a0\n");
             return;
        default:
        break;
    }

    if(AST->Kind == ND_NUM) {
        printf("  li a0, %d\n", AST->Val);
        return;    
    }
    genExpr(AST->RHS);
    push();//right tree res push into a0 
    genExpr(AST->LHS);
    pop("a1");//left tree pop to a1
    
    switch(AST->Kind) {
        case ND_ADD:
            printf("  add a0, a0, a1\n");
            return;
        case ND_SUB:
            printf("  sub a0, a0, a1\n");
            return;
        case ND_MUL:
            printf("  mul a0, a0, a1\n");
            return;
        case ND_DIV:
            printf("  div a0, a0, a1\n");
            return;
        default:
            break;
    }
    error("invalid expression!");    
}

int main(int Argc, const char** Argv) {

    if(Argc != 2) {
        
        error("%s: invalid number of Arguments!\n", Argv[0]);
        // 1 represent error
    }
    currentInput = Argv[1];
    Token* Tok = tokenize();
    printf("  .globl main\n");
    printf("main:\n");
    Node* Nd = expr(&Tok, Tok); 
    genExpr(Nd);
    
    printf("  ret\n");
    
    assert(Depth == 0);
    return 0;
}

   
