//===- RISCVDAGBuilder.h ---------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCVDAGBuilder class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_RISCVDAGBuilder_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_RISCVDAGBuilder_H

#include "RISCVDAGRaisingInfo.h"
#include "RISCVFunctionRaisingInfo.h"

using namespace llvm;

/// This is to build DAG for each Function by analyzing MachineInstructions.
class RISCVDAGBuilder {
public:
  RISCVDAGRaisingInfo &DAGInfo;
  SelectionDAG &DAG;
  /// FuncInfo - Information about the function as a whole.
  RISCVFunctionRaisingInfo &FuncInfo;
  RISCVDAGBuilder(RISCVDAGRaisingInfo &dagInfo, RISCVFunctionRaisingInfo &funcInfo)
      : DAGInfo(dagInfo), DAG(dagInfo.getCurDAG()), FuncInfo(funcInfo) {}
  /// visit - Collects the information of each MI to create SDNodes.
  void visit(const MachineInstr &mi);

private:
  /// Analyzes CPSR register information of MI to collect conditional code
  /// properties.
  void visitCC(const MachineInstr &mi, MachineSDNode *mnode);
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_RISCVDAGBuilder_H
