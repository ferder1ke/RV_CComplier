/* ************************************************************************
> File Name:     main.c
> Author:        ferdi
> 邮箱:          22s121106@stu.hit.edu.cn
> Created Time:  Wed 31 Jan 2024 03:11:53 PM CST
> Description:   
 ************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>

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
static bool equal(Token* Tok, char* str){
    return memcmp(Tok->Pos, str, Tok->Len) == 0 && str[Tok->Len] == '\0';
}

static Token* skip(Token* Tok, char* str) {
    if(!equal(Tok, str))
        error("expect '%s'", str);
    return Tok->Next;
} 

static int getNum(Token* Tok) {
    if(Tok->Kind != TK_NUM)
        error("expect number!");
    return Tok->Val;
}

static Token* genToken(TokenKind Kind, char* Start, char* End) {
    Token* Tok = calloc(1, sizeof(Token));
    Tok->Kind = Kind;
    Tok->Pos = Start;
    Tok->Len = End - Start;
    return Tok;
}

static Token* parseToken(char* P) {
   Token Head = {};
   Token* cur = &Head;
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

       if(*P == '+' || *P == '-') {
            cur->Next = genToken(TK_PUNCT, P, P + 1);
            cur = cur->Next;
            ++P;
            continue;
       }
       
       error("unexpected character %s", P);
   }
   
   cur->Next = genToken(TK_EOF, P, P);
   return Head.Next;
}


int main(int Argc, const char** Argv) {

    if(Argc != 2) {
        
        error("%s: invalid number of Arguments!\n", Argv[0]);
        // 1 represent error
    }
    Token* Tok = parseToken(Argv[1]);
    printf("  .globl main\n");
    printf("main:\n");
    //num (op num) (op num)...
    printf("  li a0, %d\n", getNum(Tok));
    Tok = Tok->Next;
    while(Tok->Kind != TK_EOF) {
         if(equal(Tok, "+")) {
            Tok = Tok->Next;
            printf("  addi a0, a0, %d\n", getNum(Tok));
            Tok = Tok->Next;
            continue;
         }else if(equal(Tok, "-")) {
            Tok = Tok->Next;
            printf("  addi a0, a0, -%d\n", getNum(Tok));
            Tok = Tok->Next;
            continue;
         }
    }
    printf("  ret\n");

    return 0;
}

   
