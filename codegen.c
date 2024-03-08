/* ************************************************************************
> File Name:     codegen.c
> Author:        ferdi
> Created Time:  Mon 04 Mar 2024 10:32:14 PM CST
> Description:   
 ************************************************************************/
#include "rvcc.h"

static int Count() {
    static int I = 1;
    return I++;
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

static int getAddr(Node* Nd) {
    if(Nd->Kind == ND_VAR) {
        printf("  addi a0, fp, %d\n", Nd->Var->Offset);
        return;
    }
    error("not an lvalue");
}

static int alignTo(int N, int Align) {
    return (N + Align - 1) / Align * Align;
}

static void assignLVarOffsets(Function* prog) {
    int Offset = 0;
    for(Obj* Var = prog->Locals; Var; Var = Var->Next){
        Offset += 8;
        Var->Offset = -Offset;
    }
    prog->StackSize = alignTo(Offset, 16);
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
  switch (Nd->Kind) {
  // 生成return语句
  case ND_RETURN:
    genExpr(Nd->LHS);
    // 无条件跳转语句，跳转到.L.return段
    // j offset是 jal x0, offset的别名指令
    printf("  j .L.return\n");
    return;
  // 生成表达式语句
  case ND_FOR: {
    int C = Count();

    genStmt(Nd->Init);
    printf(".L.begin.%d:\n", C);
    
    if(Nd->Cond) {
        genExpr(Nd->Cond);
        printf("  beqz a0, .L.end.%d\n", C);
    }
    
    genStmt(Nd->Then);
    
    if(Nd->Inc) {
        genExpr(Nd->Inc);
    }
    
    printf("  j .L.begin.%d\n", C);
    printf(".L.end.%d:\n", C);
    return;
  }

  case ND_IF: {
    int C = Count();

    genExpr(Nd->Cond);
    printf("  beqz a0, .L.else.%d\n", C);
    genStmt(Nd->Then);
    printf("  j .L.end.%d\n", C);
    printf(".L.else.%d:\n", C);
    if(Nd->Els){
        genStmt(Nd->Els);
    }
    printf(".L.end.%d:\n", C);
    return;
  }

  case ND_EXPR_STMT:
    genExpr(Nd->LHS);
    return;
  case ND_BLOCK:
    for(Node* Cur = Nd->Body; Cur; Cur = Cur->Next)
        genStmt(Cur);
    return;
  default:
    break;
  }
  error("invalid statement");
}

void codegen(Function* prog) {
    assignLVarOffsets(prog);
    printf("  .globl main\n");
    printf("main:\n");
    printf("  addi sp, sp, -8\n");
    printf("  sd fp, 0(sp)\n");
    printf("  mv fp, sp\n");

    printf("  addi sp, sp, -%d\n", prog->StackSize);
    genStmt(prog->Body);
    assert(Depth == 0);
    
    printf(".L.return:\n");
    printf("  mv sp, fp\n");
    printf("  ld fp, 0(sp)\n");
    printf("  addi sp, sp, 8\n");
    printf("  ret\n");
}
