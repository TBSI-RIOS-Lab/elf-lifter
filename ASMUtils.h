//===-- ASMUtils.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//



#ifndef LLVM_TOOLS_LLVM_MCTOLL_ASMUTILS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ASMUTILS_H


#include <vector>
#include <string>
#include <set>

using namespace std;

typedef struct GlobalData{

  uint64_t addr;
  int64_t  size;
  string   name;
  int64_t  offset;

}GlobalData;


GlobalData* MatchGlobalData(uint64_t addr);

GlobalData* MatchGlobalSection(uint64_t addr);

std::string MatchPLTFunction(uint64_t addr);




#endif // LLVM_TOOLS_LLVM_MCTOLL_ASMUTILS_H