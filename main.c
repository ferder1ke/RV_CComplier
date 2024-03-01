/* ************************************************************************
> File Name:     main.c
> Author:        ferdi
> 邮箱:          22s121106@stu.hit.edu.cn
> Created Time:  Wed 31 Jan 2024 03:11:53 PM CST
> Description:   
 ************************************************************************/
#include <stdio.h>
#include <stdlib.h>

int main(int Argc, const char** Argv) {

    if(Argc != 2) {
        
        fprintf(stderr, "%s: invalid number of Arguments!\n", Argv[0]);

        // 1 represent error
        return 1;
    }
    printf("  .globl main\n");
    // main段标签
    printf("main:\n");
    
    char* Pos = Argv[1];//args list pointer
    //num (op num) (op num)...
    
    // li为addi别名指令，加载一个立即数到寄存器中
    // 传入程序的参数为str类型，因为需要转换为需要int类型，
    // atoi为“ASCII to integer”
    printf("  li a0, %ld\n", strtol(Pos, &Pos, 10));

    while(*Pos != '\0') {
        if(*Pos == '+') {
            ++Pos;
            printf("  addi a0, a0, %ld\n", strtol(Pos, &Pos, 10));
            continue;
        }else if(*Pos == '-') {
            ++Pos;
            printf("  addi a0, a0, -%ld\n", strtol(Pos, &Pos, 10));
            continue;
        }
        fprintf(stderr, "unexpected character:'%c'\n", *Pos);
        return 1;
    }
    // ret为jalr x0, x1, 0别名指令，用于返回子程序
    printf("  ret\n");

    return 0;
}

   
