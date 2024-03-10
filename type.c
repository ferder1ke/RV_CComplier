/* ************************************************************************
> File Name:     type.c
> Author:        ferdi
> Description:   
 ************************************************************************/
#include "rvcc.h"

Type* TypeInt = &(Type){
    TypeINT
};

bool isInteger(Type* TY) {
    return TY->typeKind == TypeINT;
}

Type* pointerTo(Type* Base) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = TypePTR;
    Ty->Base = Base;
    return Ty;
}
Type* funcType(Type* ReturnTy) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = TypeFunc;
    Ty->ReturnTy = ReturnTy;
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

    switch(Nd->Kind) {
        case ND_ADD:
        case ND_SUB:
        case ND_MUL:
        case ND_DIV:
        case ND_NEG:
        case ND_ASSIGN:
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

        case ND_ADDR:
            Nd->Ty = pointerTo(Nd->LHS->Ty);
            return;
        case ND_DEREF: 
            if(Nd->LHS->Ty->typeKind != TypePTR)
                errorTok(Nd->Tok, "invalid pointer dereference");
            Nd->Ty = Nd->LHS->Ty->Base;
            return;
        case ND_FUNCALL:
            Nd->Ty = TypeInt;
        default:
            break;
    }
}

