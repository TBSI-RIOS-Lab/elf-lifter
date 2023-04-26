//===-- AssemblyCFG.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYCFG_H
#define LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYCFG_H

#include "AssemblyBasicBlock.h"
#include <vector>
#include <string>
#include <unordered_set>
#include "PHI.h"
#include "AssemblyFunction.h"

//#include "AssemblyInstruction.h"


using namespace llvm;
using namespace std;

class AssemblyBasicBlock;
class AssemblyFunction;
class AssemblyInstruction;

class AssemblyInstruction;

class AssemblyCFG {
private:
  int Size;
  string FunctionName;
  AssemblyFunction* Function;
  uint64_t startAddress;
  uint64_t endAddress;
  int startOffset;
  vector<AssemblyBasicBlock*> BasicBlocks;
  vector<AssemblyInstruction*> Prologue = vector<AssemblyInstruction*>(0);
  vector<AssemblyInstruction*> Epilogue = vector<AssemblyInstruction*>(0);
  
  // The stack includes Prologue & Epilogue
  int phonystacksize;
  
  // The stack excludes Prologue & Epilogue
  int stacksize;

  AssemblyCFG(const AssemblyCFG &) = delete;


public:
  ~AssemblyCFG();
  AssemblyCFG(AssemblyFunction* fuction, uint64_t start_address, int start_offset, uint64_t end_address);
  int addBasicBlock(AssemblyBasicBlock* ass_bb);
  void AddFunction();
  AssemblyFunction* getFunction();

  int getSize() const;
  string getName() const;
  vector<AssemblyBasicBlock*> &getBasicBlocks();
  vector<AssemblyBasicBlock*>::iterator begin();
  vector<AssemblyBasicBlock*>::iterator end();
  uint64_t getStartAddress() const;
  uint64_t getEndAddress() const;
  int getStartOffset();
  void dump();

  int FindPrologue();
  int FindEpilogue(); 
  int FindMatchedEpilogue(int reg, int &size); 
  int FindRet();

  AssemblyInstruction*  FindDataSource(AssemblyInstruction* inst, std::unordered_set<AssemblyInstruction*> visited);
  void TraverseLoadStore();



  void setPhonyStack(int size);
  int  getPhonyStack();
  void setRealStack(int size);
  int  getRealStack();

  GlobalData* ComputeGlobalAddr(AssemblyInstruction* lui);

  void ProcessFuncCall();


  void ProcessRISCVGP();  
  
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYCFG_H