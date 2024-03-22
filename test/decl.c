/* ************************************************************************
> File Name:     decl.c
> Author:        程序员Carl
> 微信公众号:    代码随想录
> Created Time:  2024年03月21日 星期四 03时25分14秒
> Description:   
 ************************************************************************/
#include "test.h"

int main() {
  // [62] 修正解析复杂类型声明
  
  // [72] 支持_Bool类型
  ASSERT(0, ({ _Bool x=0; x; }));
  ASSERT(1, ({ _Bool x=1; x; }));
  ASSERT(1, ({ _Bool x=2; x; }));
  ASSERT(1, (_Bool)1);
  ASSERT(1, (_Bool)2);
  ASSERT(0, (_Bool)(char)256);
  ASSERT(8, ({ long long x; sizeof(x); }));

  ASSERT(1, ({ char x; sizeof(x); }));
  ASSERT(2, ({ short int x; sizeof(x); }));
  ASSERT(2, ({ int short x; sizeof(x); }));
  ASSERT(4, ({ int x; sizeof(x); }));
  ASSERT(8, ({ long int x; sizeof(x); }));
  ASSERT(8, ({ int long x; sizeof(x); }));

  printf("OK\n");
  return 0;
}
