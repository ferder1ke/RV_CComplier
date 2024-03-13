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

static bool isKeyWord(Token* Tok) {
    static char* Kw[] = {"return", "if", "else", "for" ,"while", "int", "sizeof", "char"};
    for(int i = 0; i < sizeof(Kw) / sizeof(*Kw); i++) {
        if(equal(Tok, Kw[i]))
            return true;
    }
    return false;
}

static void convertKeywords (Token* Tok) {
    for(Token* Cur = Tok; Cur->Kind != TK_EOF; Cur = Cur->Next) {
        if(isKeyWord(Cur)) {
            Cur->Kind = TK_KEYWORD;
        }
    }
}

Token* skip(Token* Tok, char* str) {
    if(!equal(Tok, str))
        errorTok(Tok, "expect '%s'", str);
    return Tok->Next;
} 

bool consume(Token** Rest, Token* Tok, char* Str) {
    if(equal(Tok, Str)){
        *Rest = Tok->Next;
        return true;
    }
    *Rest = Tok;    
    return false;
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

static int readEscapeChar(char** NewPos, char* P) {
   if('0' <= *P && *P <= '7'){ //octal num hint:the octal num len can`t beyond 3 
        int C = *P - '0';
        ++P;
        if('0' <= *P && *P <= '7'){ //octal num hint:the octal num len can`t beyond 3 
            C = (C << 3) + (*P - '0');
            ++P;
            if('0' <= *P && *P <= '7'){ //octal num hint:the octal num len can`t beyond 3 
                C = (C << 3) + (*P - '0');
                ++P;
            }
        }
    *NewPos = P;
    return C;
   }
    *NewPos = P + 1;
    switch (*P) {
        case 'a': // 响铃（警报）
            return '\a';
        case 'b': // 退格
            return '\b';
        case 't': // 水平制表符，tab
            return '\t';
        case 'n': // 换行
            return '\n';
        case 'v': // 垂直制表符
            return '\v';
        case 'f': // 换页
            return '\f';
        case 'r': // 回车
            return '\r';
        // 属于GNU C拓展
        case 'e': // 转义符
            return 27;
        default: // 默认将原字符返回
            return *P;
    }
}
static char* stringLiteralEnd(char* P) {
    char* Start = P;
    for(; *P != '"'; ++P) {
        if(*P == '\n' || *P == '\0')
            errorAt(Start, "unclosed string literal");
        if(*P == '\\')
            ++P;
    }
    return P;
}


static Token* readStringLiteral(char* Start) {
    char* End = stringLiteralEnd(Start + 1);
    char* Buf = calloc(1, End - Start);
    int Len = 0;

    for(char* P = Start + 1; P < End;) {
        if(*P == '\\') {
            Buf[Len++] = readEscapeChar(&P, P + 1);
        }else {
            Buf[Len++] = *P++;
        }
    }
    Token* Tok = genToken(TK_STR, Start, End + 1); 
    Tok->Ty = arrayof(TypeChar, Len + 1);//don`t forget the '\0'
    Tok->Str = Buf;
    return Tok;
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
       if(*P == '"') {
            cur->Next = readStringLiteral(P);
            cur = cur->Next;
            P += cur->Len;
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
   convertKeywords(Head.Next); 
   return Head.Next;
}


