/* ************************************************************************
> File Name:     test/literal.c
> Author:        程序员Carl
> 微信公众号:    代码随想录
> Created Time:  2024年03月23日 星期六 03时05分19秒
> Description:   
 ************************************************************************/
#include "test.h"

int main() {
  // [73] 支持字符字面量
  ASSERT(511, 0777);
  ASSERT(0, 0x0);
  ASSERT(10, 0xa);
  ASSERT(10, 0XA);
  ASSERT(48879, 0xbeef);
  ASSERT(48879, 0xBEEF);
  ASSERT(48879, 0XBEEF);
  ASSERT(0, 0b0);
  ASSERT(1, 0b1);
  ASSERT(47, 0b101111);
  ASSERT(47, 0B101111);
  ASSERT(97, 'a');
  ASSERT(10, '\n');
  ASSERT(127, '\x7f');

  printf("OK\n");
  return 0;
}
