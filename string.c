/* ************************************************************************
> File Name:     string.c
> Author:        ferdi
> Created Time:  2024年03月13日 星期三 11时21分24秒
> Description:   
 ************************************************************************/
#include "rvcc.h"

char* format(char* Fmt, ...) {
    char* Buf;
    size_t BufLen;

    FILE* Out = open_memstream(&Buf, &BufLen);

    va_list VA;
    va_start(VA, Fmt);

    vfprintf(Out, Fmt, VA);
    va_end(VA);
    
    fclose(Out);
    return Buf;
}
