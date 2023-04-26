; ModuleID = '/home/ychen/LLVM/llvm/tools/llvm-mctoll/test/dyntest//caller-riscv.o'
source_filename = "/home/ychen/LLVM/llvm/tools/llvm-mctoll/test/dyntest//caller-riscv.o"

define i32 @main() {
EntryBlock:
  %stack.3 = alloca i32, align 4
  %0 = alloca i1, align 32
  store i1 false, i1* %0, align 1
  %1 = alloca i1, align 32
  store i1 false, i1* %1, align 1
  %2 = alloca i1, align 32
  store i1 false, i1* %2, align 1
  %3 = alloca i1, align 32
  store i1 false, i1* %3, align 1
  %4 = add i32 3, 0
  %5 = call i32 @libfunc(i32 %4)
  store i32 %5, i32* %stack.3, align 2
  %6 = load i32, i32* %stack.3, align 2
  %7 = add i32 %6, 0
  br label %8

8:                                                ; preds = %EntryBlock
  %9 = phi i32 [ %7, %EntryBlock ]
  ret i32 %9
}

declare dso_local i32 @libfunc(i32)
