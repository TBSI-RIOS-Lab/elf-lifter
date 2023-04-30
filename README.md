# Introduction

This tool statically (AOT) translates (or raises) binaries to LLVM IR.

## Building as part of the LLVM tree

```sh
cd llvm-project && git clone https://github.com/TBSI-RIOS-Lab/elf-lifter llvm/tools/elf-lifter

mkdir build && cd build
cmake -G "Ninja" -DLLVM_TARGETS_TO_BUILD="X86;ARM" -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_ENABLE_ASSERTIONS=true -DCMAKE_BUILD_TYPE=Release ../llvm
ninja 
```


# Usage

```sh
llvm-mctoll <ELF>
```

| Command | Description |
| --- | --- |
| `-dh` or `--help` |  Display available options |
| `-d <binary>` | Generate LLVM IR for a binary and place the result in `<binary>-dis.ll` |
| `--filter-functions-file=<file>` | Text file with C functions to exclude or include during raising |
| `--include-files=[file1,file2,file3,...]` or  `-I file1 -I file2 -I file3` | Specify full path of one or more files with function prototypes to use|
| `-debug` | Print all debug output |
| `-debug-only=mctoll` | Print the LLVM IR after each pass of the raiser |
| `-debug-only=prototypes` | Print ignored duplicate function prototypes in --include-files |


See [usage document](./doc/Usage.md) for additional details of command-line options.

## Checking correctness of translation

The easiest way to check the raised LLVM IR `<binary>-dis.ll` is correct is to compile the IR to an executable using `clang` and run the resulting executable. The tests in the repository follow this methodology. 

