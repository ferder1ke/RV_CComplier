#-g debug flag
CFLAGS=-std=c11 -g -fno-common

CC=gcc

rvcc: main.o
	$(CC) -o rvcc $(CFLAGS) main.o
test: rvcc
	./test.sh

# 清理标签，清理所有非源代码文件
clean:
	rm -f rvcc *.o *.s tmp* a.out

# 伪目标，没有实际的依赖文件
.PHONY: test clean
