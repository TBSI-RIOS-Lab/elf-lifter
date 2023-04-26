//===- RISCVFunctionRaisingInfo.cpp - Binary raiser utility llvm-mctoll --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementaion of RISCVFunctionRaisingInfo class for use
// by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCVFunctionRaisingInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"

using namespace llvm;
#define DEBUG_TYPE "mctoll"

/// Initialize this RISCVFunctionRaisingInfo with the given Function and its
/// associated MachineFunction.
void RISCVFunctionRaisingInfo::set(RISCV32ModuleRaiser &mr, Function &fn,
                              MachineFunction &mf, SelectionDAG *dag) {
  MR = &mr;
  Fn = &fn;
  MF = &mf;
  CTX = dag->getContext();
  DLT = &MR->getModule()->getDataLayout();

  DefaultType = Type::getIntNTy(*CTX, DLT->getPointerSizeInBits());
}

SDValue RISCVFunctionRaisingInfo::getValueByRegister(unsigned reg) {
  assert((RegValMap.count(reg) != 0) &&
         "Can not find the corresponding value!");
  return SDValue(RegValMap[reg], 0);
}

void RISCVFunctionRaisingInfo::setValueByRegister(unsigned reg, SDValue val) {
  assert((val.getNode() != nullptr) && "Can not map a nullptr to a register!");
  LLVM_DEBUG(dbgs()<<"registering setValuebyRegister::"<<reg<<"<-"<<val.getNode()->PersistentId<<"\n");
  RegValMap[reg] = val.getNode();
}

SDValue RISCVFunctionRaisingInfo::getValFromRegMap(SDValue val) {
  unsigned reg = static_cast<RegisterSDNode *>(val.getNode())->getReg();
  return (RegValMap.count(reg) == 0) ? val : SDValue(RegValMap[reg], 0);
}

/// Clear out all the function-specific state. This returns this
/// RISCVFunctionRaisingInfo to an empty state, ready to be used for a
/// different function.
void RISCVFunctionRaisingInfo::clear() {
  MBBMap.clear();
  ValueMap.clear();
  VisitedBBs.clear();
  RegValMap.clear();
  ArgValMap.clear();
  NodeRegMap.clear();
  AllocaMap.clear();
  RetValMap.clear();
}

/// Get the corresponding BasicBlock of given MachineBasicBlock.
BasicBlock *RISCVFunctionRaisingInfo::getBasicBlock(MachineBasicBlock &mbb) {
  for (auto bb : MBBMap) {
    if (bb.second == &mbb)
      return const_cast<BasicBlock *>(bb.first);
  }

  return nullptr;
}

/// Get the corresponding BasicBlock of given MachineBasicBlock.
/// If does not give a MachineBasicBlock, it will create a new BasicBlock
/// on current Function, and returns it.
BasicBlock *RISCVFunctionRaisingInfo::getOrCreateBasicBlock(MachineBasicBlock *mbb) {
  Function *fn = getCRF();
  if (mbb == nullptr)
    return BasicBlock::Create(fn->getContext(), "", fn);

  BasicBlock *bb = getBasicBlock(*mbb);
  if (bb != nullptr)
    return bb;

  if (&MF->front() == mbb)
    bb = &fn->getEntryBlock();
  else
    bb = BasicBlock::Create(fn->getContext(), "", fn);

  MBBMap.insert(std::make_pair(bb, mbb));

  return bb;
}
