// RUN: clang %S/../Inputs/simple-phi.c -o %t.so --target=%arm_triple -fuse-ld=lld -shared
// RUN: llvm-mctoll -d %t.so
// RUN: clang -o %t1 %s %t-dis.ll -mx32
// RUN: %t1 2>&1 | FileCheck %s
// CHECK: true or true = true
// CHECK-NEXT: true or false = true
// CHECK-NEXT: false or true = true
// CHECK-NEXT: false or false = false

#include <stdio.h>
typedef int bool;
#define true 1
#define false 0

extern bool orvalues(bool r, bool y);

int main() {
  printf("true or true = %s\n", (orvalues(true, true) ? "true" : "false"));
  printf("true or false = %s\n", (orvalues(true, false) ? "true" : "false"));
  printf("false or true = %s\n", (orvalues(false, true) ? "true" : "false"));
  printf("false or false = %s\n", (orvalues(false, false) ? "true" : "false"));
  return 0;
}
