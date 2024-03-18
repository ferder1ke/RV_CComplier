/* ************************************************************************
> File Name:     union.c
> Author:        程序员Carl
> 微信公众号:    代码随想录
> Created Time:  2024年03月19日 星期二 04时46分47秒
> Description:   
 ************************************************************************/
#include "test.h"

int main() {
  // [54] 支持union
  ASSERT(8, ({ union { int a; char b[6]; } x; sizeof(x); }));
  ASSERT(3, ({ union { int a; char b[4]; } x; x.a = 515; x.b[0]; }));
  ASSERT(2, ({ union { int a; char b[4]; } x; x.a = 515; x.b[1]; }));
  ASSERT(0, ({ union { int a; char b[4]; } x; x.a = 515; x.b[2]; }));
  ASSERT(0, ({ union { int a; char b[4]; } x; x.a = 515; x.b[3]; }));

  printf("OK\n");
  return 0;
}
