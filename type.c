/* ************************************************************************
> File Name:     type.c
> Author:        ferdi
> Description:   
 ************************************************************************/
#include "rvcc.h"

Type* TypeInt = &(Type){
    TypeINT,
    8
};

bool isInteger(Type* TY) {
    return TY->typeKind == TypeINT;
}

Type* copyType(Type* Ty) {
    Type* Ret = calloc(1, sizeof(Type));
    *Ret = *Ty;
    return Ret;
}

Type* pointerTo(Type* Base) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = TypePTR;
    Ty->Base = Base;
    Ty->Size = 8;
    return Ty;
}

Type* funcType(Type* ReturnTy) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = TypeFunc;
    Ty->ReturnTy = ReturnTy;
    return Ty;
}

Type* arrayof(Type* Base, int Len) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = TypeARRAY;
    Ty->Base = Base;
    Ty->Size = Base->Size * Len;
    Ty->ArraryLen = Len;

    return Ty;
}

void addType(Node* Nd) {
    if(!Nd || Nd->Ty) {
        return;
    }

    addType(Nd->LHS);
    addType(Nd->RHS);
    addType(Nd->Cond);
    addType(Nd->Then);
    addType(Nd->Els);
    addType(Nd->Init);
    addType(Nd->Inc);
   
    for(Node* Cur = Nd->Body; Cur; Cur = Cur->Next) {
        addType(Cur);
    }

    for(Node* Cur = Nd->Args; Cur; Cur = Cur->Next) {
        addType(Cur);
    }

    switch(Nd->Kind) {
        case ND_ADD:
        case ND_SUB:
        case ND_MUL:
        case ND_DIV:
        case ND_NEG:
            Nd->Ty = Nd->LHS->Ty;
            return;
        case ND_ASSIGN:
            if(Nd->LHS->Ty->typeKind == TypeARRAY)
                errorTok(Nd->LHS->Tok, "not an lvalue");
            Nd->Ty = Nd->LHS->Ty;
            return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
        case ND_NUM:
            Nd->Ty = TypeInt;
            return;

        case ND_VAR:
            Nd->Ty = Nd->Var->Ty;
            return;

        case ND_ADDR: {
            Type* Ty = Nd->LHS->Ty;
            if(Ty->typeKind == TypeARRAY) {
                Nd->Ty = pointerTo(Ty->Base); 
            }else
                Nd->Ty = pointerTo(Ty);
            return;
        }

        case ND_DEREF: 
            if(!Nd->LHS->Ty->Base)
                errorTok(Nd->Tok, "invalid pointer dereference");
            Nd->Ty = Nd->LHS->Ty->Base;
            return;
        case ND_FUNCALL:
            Nd->Ty = TypeInt;
        default:
            break;
    }
}

