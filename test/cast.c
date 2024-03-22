/* ************************************************************************
> File Name:     ./test/cast.c
> Author:        程序员Carl
> 微信公众号:    代码随想录
> Created Time:  2024年03月22日 星期五 08时30分34秒
> Description:   
 ************************************************************************/
#include "test.h"

int main() {
  // [67] 支持类型转换
  ASSERT(131585, (int)8590066177);
  ASSERT(513, (short)8590066177);
  ASSERT(1, (char)8590066177);
  ASSERT(1, (long)1);
  ASSERT(0, (long)&*(int *)0);
  ASSERT(513, ({ int x=512; *(char *)&x=1; x; }));
  ASSERT(5, ({ int x=5; long y=(long)&x; *(int*)y; }));

  (void)1;

  printf("OK\n");
  return 0;
}
