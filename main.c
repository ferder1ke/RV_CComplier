/* ************************************************************************
> File Name:     main.c
> Author:        ferdi
> 邮箱:          22s121106@stu.hit.edu.cn
> Created Time:  Wed 31 Jan 2024 03:11:53 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"

//Obj file Path
static char* OptO;

//Input file Path
static char* InputPath;

static void usage(int Status) {
    fprintf(stderr, "rvcc [ -o <path> ] <file>\n");
    exit(Status);
}

static void parseArgs(int Argc, char** Argv) {
    for(int I = 1; I < Argc; ++I) {
        if(!strcmp(Argv[I], "--help"))
            usage(0);

        // -o XXX
        if(!strcmp(Argv[I], "-o")) {
           if(!Argv[++I])
               usage(1);
           OptO = Argv[I];
           continue;
        }
       
        // -oXXX
        if(Argv[I][0] == '-' && Argv[I][1] != '\0') {
            error("unknown argument: %s", Argv[I]);
        }
        
        InputPath = Argv[I];
    }
    if(!InputPath)
        error("no Input File");
}

static FILE* openFile(char* Path) {
    if(!Path || strcmp(Path, "-") == 0)
        return stdout;

    FILE* Out = fopen(Path, "w");
    if(!Out) {
        error("cannot open output file: %s: %s", Path, strerror(errno));
    }
    return Out;
}

int main(int Argc, const char** Argv) {

    parseArgs(Argc, Argv); 
    Token* Tok = tokenizeFile(InputPath);
    Obj* Prog = parse(Tok); 
    FILE* Out = openFile(OptO);
    fprintf(Out, ".file 1 \"%s\"\n", InputPath);
    codegen(Prog, Out);
    
    return 0;
}

   
