// RUN: clang %S/../Inputs/test-1.c -o %t.so --target=%arm_triple -fuse-ld=lld -shared
// RUN: llvm-mctoll -d %t.so
// RUN: clang  -o %t1 %s %t-dis.ll -mx32
// RUN: %t1 2>&1 | FileCheck %s
// CHECK: test_1_func result 5

#include <stdio.h>

extern long test_1_func(int a, long b);

int main() {
  printf("test_1_func result %ld\n", test_1_func(2, 3));
  return 0;
}
