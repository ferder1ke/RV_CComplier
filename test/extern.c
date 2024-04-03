/* ************************************************************************
> File Name:     test/extern.c
> Author:        程序员Carl
> 微信公众号:    代码随想录
> Created Time:  2024年04月04日 星期四 02时57分09秒
> Description:   
 ************************************************************************/

#include "test.h"

// [116] 支持extern
extern int ext1;
extern int *ext2;

int main() {
  // [116] 支持extern
  ASSERT(5, ext1);
  ASSERT(5, *ext2);

  printf("OK\n");
  return 0;
}
