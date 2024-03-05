/* ************************************************************************
> File Name:     tokenize.c
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:25:36 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"

/*Lexical analysis*/
static char* currentInput;
void error(char* Fmt, ...) {
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

void errorAt(char* Loc, char* Fmt, ...) {
    va_list VA;
    va_start(VA, Fmt);
    __verrorAt(Loc, Fmt, VA);
    exit(1);
}
void errorTok(Token* Tok, char* Fmt, ...) {
    va_list VA;
    va_start(VA, Fmt);
    __verrorAt(Tok->Pos, Fmt, VA);
    exit(1);
}

bool equal(Token* Tok, char* str){
    return memcmp(Tok->Pos, str, Tok->Len) == 0 && str[Tok->Len] == '\0';
}

static bool startWith(char* str, char* subStr) {
    return strncmp(str, subStr, strlen(subStr)) == 0;
}

static int readPunct(char* P) {
    if(startWith(P, "==") ||
       startWith(P, "!=") ||
       startWith(P, ">=") ||
       startWith(P, "<="))
        return 2;
    return ispunct(*P) ? 1 : 0;
}

Token* skip(Token* Tok, char* str) {
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

static bool isIdent1(char C) {
    return ('a' <= C && C <= 'z') || ('A' <= C && C <= 'Z') || (C == '_');
} 

static bool isIdent2(char C) {
    return isIdent1(C) || ('0' <= C && C <= '9');
}
Token* tokenize(char* P) {
   currentInput = P;
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
       
       if(isIdent1(*P)) {
           char* dst = P;
           do{
               ++P;
           }while(isIdent2(*P));
           cur->Next = genToken(TK_IDENT, dst, P);
           cur = cur->Next;
           continue;
       }

       int punctLen = readPunct(P);
       if(punctLen) {
            cur->Next = genToken(TK_PUNCT, P, P + punctLen);
            cur = cur->Next;
            P += punctLen;
            continue;
       }
       
       errorAt(P, "invalid Token");
       
       error("unexpected character %s", P);
   }
   
   cur->Next = genToken(TK_EOF, P, P);
   return Head.Next;
}


