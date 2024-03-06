/* ************************************************************************
> File Name:     codegen.c
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:32:14 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"
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

static int getAddr(Node* Nd) {
    if(Nd->Kind == ND_VAR) {
        int offset = (Nd->Name - 'a' + 1) * 8;
        printf("  addi a0, fp, %d\n", -offset);
        return;
    }
    error("not an lvalue");
}

static void genExpr(Node* AST) {
    switch(AST->Kind) {//Two terminator
        case ND_NUM:
            printf("  li a0, %d\n", AST->Val);
            return;
        case ND_NEG:
             genExpr(AST->LHS);
             printf("  neg a0, a0\n");
             return;
        case ND_ASSIGN:
            getAddr(AST->LHS);
            push();
            genExpr(AST->RHS);
            pop("a1"); 
            printf("  sd a0, 0(a1)\n");
            return;
        case ND_VAR:
            getAddr(AST);
            printf("  ld a0, 0(a0)\n");
            return;
        default:
        break;
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
        case ND_EQ:
        case ND_NE:
            printf("  xor a0, a0, a1\n");
            if(AST->Kind == ND_EQ) 
                printf("  seqz a0, a0\n");
            else 
                printf("  snez a0, a0\n");
            
            return;
        case ND_LT:
            printf("  slt a0, a0, a1\n");
            return;
        case ND_LE:
            printf("  slt a0, a1, a0\n");
            printf("  xori a0, a0, 1\n");
            return;
        default:
            break;
    }
    error("invalid expression!");    
}
static void genStmt(Node *Nd) {
    if(Nd->Kind == ND_EXPR_STMT) {
        genExpr(Nd->LHS);
        return;
    }
    error("invalid statement");
}
void codegen(Node *Nd) {
    printf("  .globl main\n");
    printf("main:\n");
    printf("  addi sp, sp, -8\n");
    printf("  sd fp, 0(sp)\n");
    printf("  mv fp, sp\n");

    printf("  addi sp, sp, -208\n");
    for(Node* N = Nd; N; N = N->Next){
        genStmt(N);
        assert(Depth == 0);
    }
    printf("  mv sp, fp\n");
    printf("  ld fp, 0(sp)\n");
    printf("  addi sp, sp, 8\n");
    printf("  ret\n");
}
