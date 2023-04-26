//===-- AssemblyFunction.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYFUNCTION_H
#define LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYFUNCTION_H

#include <vector>
#include <string>

using namespace std;


// class AssemblyCFG;

class AssemblyFunction {
private:
  string FunctionName;
  string FunctionPrototype;
  uint64_t startAddress;
  uint64_t endAddress;
  int ArgumentNum;
  bool HasReturn;

  int ReturnWidth;


  // IsAddress = 1, IsData = 2
  int ReturnType;

  // Pair <Width, Signed/Unsigned+Pointer/Data>
  vector<pair<int,int>>Argument;


  AssemblyFunction(const AssemblyFunction &) = delete;


public:
  ~AssemblyFunction();
  AssemblyFunction(string fuction_name, uint64_t start_address, uint64_t end_address);
  //int getSize() const;
  string getName() const;
  string getPrototype();
  uint64_t getStartAddress() const;
  uint64_t getEndAddress() const;
  void dump();
  void setReturn(int width);
  bool hasReturn();
  void setReturnWidth(int width);
  int getReturnWidth();
  int getReturnType();
  void setReturnType(int PtrOrData);

  int getArgumentNum();
  int GetWidth(string str);
  string demangle(string name);
  void ParseArgument(string op);

  uint64_t hashCode();

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYFUNCTION_H
