//===- RISCVDAGRaisingInfo.cpp - Binary raiser utility llvm-mctoll -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementaion of RISCVDAGRaisingInfo class for use
// by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCVDAGRaisingInfo.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"
RISCVDAGRaisingInfo::RISCVDAGRaisingInfo(SelectionDAG &dag) : DAG(dag) {}

/// Gets the related IR Value of given SDNode.
Value *RISCVDAGRaisingInfo::getRealValue(SDNode *Node) {
  assert(Node != nullptr && "Node cannot be nullptr!");
  //LLVM_DEBUG(dbgs()<<"getRealValue NPMap SDNode pointer="<<(uint64_t)Node);
  assert(NPMap[Node] != nullptr &&
         "Cannot find the corresponding node proprety!");
  return NPMap[Node]->Val;
}

/// Set the related IR Value to SDNode.
void RISCVDAGRaisingInfo::setRealValue(SDNode *N, Value *V) {
  if (NPMap.count(N) == 0)
    NPMap[N] = new NodePropertyInfo();
  NPMap[N]->Val = V;
  LLVM_DEBUG(dbgs()<<"setRealValue NPMap SDNode pointer="<<(uint64_t)N->PersistentId<< "\nSDNode=");
  //LLVM_DEBUG(N->print(dbgs(),&getCurDAG()));
  LLVM_DEBUG(dbgs()<<"\nValue="<<*V<<"\n");
}

void RISCVDAGRaisingInfo::clear() {
  for (auto &elmt : NPMap)
    delete elmt.second;

  NPMap.clear();
}
