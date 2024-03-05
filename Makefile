#-g debug flag
CFLAGS=-std=c11 -g -fno-common
CC=gcc
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

rvcc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
$(OBJS): rvcc.h
test:rvcc
	./test.sh

# 清理标签，清理所有非源代码文件
clean:
	rm -f rvcc *.o *.s tmp* a.out .gdb_history build/ 
 
# 伪目标，没有实际的依赖文件
.PHONY: test clean
