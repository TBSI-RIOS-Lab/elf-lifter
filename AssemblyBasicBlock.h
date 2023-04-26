//===-- AssemblyBasicBlock.h ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYBASICBLOCK_H
#define LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYBASICBLOCK_H

#include "AssemblyInstruction.h"
#include "AssemblyFunction.h"
#include <algorithm>
#include <iostream>
#include <set>
#include <vector>
using namespace std;


class PHI;

class AssemblyBasicBlock {
private:
  int Id;
  int Size; // Bytes
  uint64_t StartAddress;
  uint64_t EndAddress;
  vector<AssemblyInstruction*> Instructions;
  set<AssemblyBasicBlock*> Predecessors;
  set<AssemblyBasicBlock*> Successors;
  AssemblyFunction* CallTarget;
  vector<PHI> phi;

  AssemblyBasicBlock(const AssemblyBasicBlock &) = delete;

public:
  ~AssemblyBasicBlock();
  AssemblyBasicBlock(int id, uint64_t start_address);
  AssemblyBasicBlock(int id, uint64_t start_address, uint64_t end_address);
  int addInstruction(AssemblyInstruction* ins);
  int addPredecessor(AssemblyBasicBlock* bb);
  int addSuccessor(AssemblyBasicBlock* bb);
  void setCallTarget(AssemblyFunction* func);
  void setEndAddress(uint64_t end_address);
  int getId() const;
  int getSize() const;
  uint64_t getStartAddress() const;
  uint64_t getEndAddress() const;
  AssemblyFunction* getCallTarget() const;
  vector<AssemblyInstruction*>* getInstructions();
  vector<AssemblyInstruction*>::iterator begin();
  vector<AssemblyInstruction*>::iterator end();
  vector<PHI>::iterator phi_begin();
  vector<PHI>::iterator phi_end();
  set<AssemblyBasicBlock*>::iterator pre_begin();
  set<AssemblyBasicBlock*>::iterator pre_end();
  set<AssemblyBasicBlock*>::iterator suc_begin();
  set<AssemblyBasicBlock*>::iterator suc_end();
  set<AssemblyBasicBlock*> getPredecessors();
  set<AssemblyBasicBlock*> getSuccessors();
  void dump();
  bool operator<(const AssemblyBasicBlock &obj) const { return Id < obj.Id; }

  int BuildLocalEdge();
  int BuildGlobalEdge();
  AssemblyInstruction* FindLocalDef(int32_t reg, vector<AssemblyInstruction*>* InstVec, int i);
  AssemblyInstruction* FindGlobalDef(int32_t reg, vector<AssemblyInstruction*>* InstVec);
  int GlobalDefUse(vector<AssemblyInstruction*>::iterator it, int RegID, 
                   vector<AssemblyInstruction*>* PreInstVec,
                   int Rs);

  void addPhi(PHI p);

  //int PhiCheck(PHI p, AssemblyInstruction* inst);

  uint64_t hashCode();

  void BuildPhiNodes();

  int RecursiveTraverse(AssemblyBasicBlock* BB,
                        vector<AssemblyInstruction*>::iterator it,
                        int Rs,
                        int RegID,
                        vector<set<int>>& visited);

  int PaintColor(int StartColor);
  void paintInsColorRecursive(AssemblyInstruction* ins, int tracedReg, int color, int type, int depth);
  string getFullMnemonic();





  int FindLoadStore();
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYBASICBLOCK_H
