/* ************************************************************************
> File Name:     type.c
> Author:        ferdi
> Description:   
 ************************************************************************/
#include "rvcc.h"

Type* TypeInt = &(Type){
    TypeINT,
    8, //8 byte
    8  // Align
};

Type* TypeChar = &(Type) {
    TypeCHAR,
    1, //1 byte
    1  // Align
};

Type* newType(TypeKind Kind, int Size, int Align) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = Kind;
    Ty->Size = Size;
    Ty->Align = Align;
    return Ty;
}

bool isInteger(Type* TY) {
    return TY->typeKind == TypeINT || TY->typeKind == TypeCHAR;
}

Type* copyType(Type* Ty) {
    Type* Ret = calloc(1, sizeof(Type));
    *Ret = *Ty;
    return Ret;
}

Type* pointerTo(Type* Base) {
    Type* Ty = newType(TypePTR, 8, 8);
    Ty->Base = Base;
    return Ty;
}

Type* funcType(Type* ReturnTy) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = TypeFunc;
    Ty->ReturnTy = ReturnTy;
    return Ty;
}

Type* arrayof(Type* Base, int Len) {
    Type* Ty = newType(TypeARRAY, Base->Size * Len, Base->Align);
    Ty->Base = Base;
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
        case ND_COMMA:
             Nd->Ty = Nd->RHS->Ty;
             return;
        case ND_MEMBER:
             Nd->Ty = Nd->Mem->Ty;
             return;
        case ND_DEREF: 
            if(!Nd->LHS->Ty->Base)
                errorTok(Nd->Tok, "invalid pointer dereference");
            Nd->Ty = Nd->LHS->Ty->Base;
            return;
        case ND_FUNCALL:
            Nd->Ty = TypeInt;
            return;
        case ND_STMT_EXPR:
            if(Nd->Body) {
                Node* Stmt = Nd->Body;
                while(Stmt->Next)
                    Stmt = Stmt->Next;
                if(Stmt->Kind == ND_EXPR_STMT){
                    Nd->Ty = Stmt->LHS->Ty;
                    return;
                }
            }
            errorTok(Nd->Tok, "statement expression returning void is not supported");
            return;
        default:
            break;
    }
}

