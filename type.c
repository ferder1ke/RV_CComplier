/* ************************************************************************
> File Name:     type.c
> Author:        ferdi
> Description:   
 ************************************************************************/
#include "rvcc.h"

Type* TypeVoid = &(Type){
    TypeVOID,
    1, // 2 byte
    1  // Align
};

Type* TypeShort = &(Type){
    TypeSHORT,
    2, // 2 byte
    2  // Align
};

Type* TypeInt = &(Type){
    TypeINT,
    4, // 4 byte
    4  // Align
};

Type* TypeLong = &(Type){
    TypeLONG,
    8, // 8 byte
    8  // Align
};

Type* TypeChar = &(Type) {
    TypeCHAR,
    1, // 1 byte
    1  // Align
};

static Type* getCommonType(Type* Ty1, Type* Ty2) {
    if(Ty1->Base)
        return pointerTo(Ty1->Base);
    if(Ty1->Size == 8 || Ty2->Size == 8)
        return TypeLong;
    return TypeInt;
}

static void usualArithConv(Node** LHS, Node** RHS) {
    Type* Ty = getCommonType((*LHS)->Ty, (*RHS)->Ty);
    *LHS = newCast(*LHS, Ty);
    *RHS = newCast(*RHS, Ty);
}

Type* newType(TypeKind Kind, int Size, int Align) {
    Type* Ty = calloc(1, sizeof(Type));
    Ty->typeKind = Kind;
    Ty->Size = Size;
    Ty->Align = Align;
    return Ty;
}

bool isInteger(Type* TY) {
    TypeKind K = TY->typeKind;
    return K == TypeINT || K == TypeCHAR || K == TypeLONG || K == TypeSHORT;
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
        case ND_NUM:
            Nd->Ty = (Nd->Val == (int)Nd->Val) ? TypeInt : TypeLong;
            return;
        case ND_ADD:
        case ND_SUB:
        case ND_MUL:
        case ND_DIV:
            usualArithConv(&Nd->LHS, &Nd->RHS);
            Nd->Ty = Nd->LHS->Ty;
            return;
        case ND_NEG: {
            Type* Ty = getCommonType(TypeInt, Nd->LHS->Ty);
            Nd->LHS = newCast(Nd->LHS, Ty);
            Nd->Ty = Ty;
            return;
        }
        case ND_ASSIGN:
            if(Nd->LHS->Ty->typeKind == TypeARRAY)
                errorTok(Nd->LHS->Tok, "not an lvalue");
            if(Nd->LHS->Ty->typeKind != TypeSTRUCT) {
                Nd->RHS = newCast(Nd->RHS, Nd->LHS->Ty);
            }
            Nd->Ty = Nd->LHS->Ty;
            return;
       
        //logical statement
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
            usualArithConv(&Nd->LHS, &Nd->RHS);
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
            if(Nd->LHS->Ty->Base->typeKind == TypeVOID)
                errorTok(Nd->Tok, "dereference a void pointer");
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

