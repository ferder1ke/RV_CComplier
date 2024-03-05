/* ************************************************************************
> File Name:     main.c
> Author:        ferdi
> 邮箱:          22s121106@stu.hit.edu.cn
> Created Time:  Wed 31 Jan 2024 03:11:53 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"
int main(int Argc, const char** Argv) {

    if(Argc != 2) {
        
        error("%s: invalid number of Arguments!\n", Argv[0]);
        // 1 represent error
    }
    Token* Tok = tokenize(Argv[1]);
    Function* Prog = parse(Tok); 
    codegen(Prog);
    
    return 0;
}

   
