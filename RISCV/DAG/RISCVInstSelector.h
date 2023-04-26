//===- RISCVInstSelector.h -------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCVInstSelector class for use
// by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_RISCVInstSelector_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_RISCVInstSelector_H

#include "RISCVDAGRaisingInfo.h"
#include "RISCVFunctionRaisingInfo.h"

using namespace llvm;

/// Does some selections on the DAG. So far, it just does the instruction
/// selection.
class RISCVInstSelector {
public:
  RISCVInstSelector(RISCVDAGRaisingInfo &dagInfo, RISCVFunctionRaisingInfo &funcInfo)
      : DAGInfo(&dagInfo), CurDAG(&dagInfo.getCurDAG()), FuncInfo(&funcInfo) {}
  void select(SDNode *N);

private:
  bool isTwoAddressMode(SDNode *node) {
    if (nullptr == node)
      return false;

    NodePropertyInfo *npi = DAGInfo->NPMap[node];

    if (nullptr == npi)
      return false;

    return npi->IsTwoAddress;
  }
  bool getAddressModule(SDNode *node);
  /// Gets the Metadata of given SDNode.
  SDValue getMDOperand(SDNode *N);
  /// Record the new defined Node, it uses to map the register number to Node.
  /// In DAG emitter, emitter get a value of use base on this defined Node.
  void recordDefinition(SDNode *oldNode, SDNode *newNode);
  /// Replace all uses of F with T, then remove F from the DAG.
  void replaceNode(SDNode *F, SDNode *T);
  /// Checks the SDNode is a function argument or not.
  bool isArgumentNode(SDNode *node);
  /// Checks the SDNode is a function return or not.
  bool isReturnNode(SDNode *node);
  /// Instruction opcode selection.
  void selectCode(SDNode *N);
  EVT getDefaultEVT() { return EVT::getEVT(FuncInfo->getDefaultType()); }

  RISCVDAGRaisingInfo *DAGInfo;
  SelectionDAG *CurDAG;
  RISCVFunctionRaisingInfo *FuncInfo;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_RISCVInstSelector_H
