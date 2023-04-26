//===- RISCV32SelectionDAGISel.h ------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCV32SelectionDAGISel class for
// use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32SELECTIONDAGISEL_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32SELECTIONDAGISEL_H

#include "RISCV32RaiserBase.h"
#include "RISCVDAGBuilder.h"
#include "RISCVDAGRaisingInfo.h"
#include "RISCVFunctionRaisingInfo.h"
#include "RISCVIREmitter.h"
#include "RISCVInstSelector.h"
#include "ModuleRaiser.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"

/// This is responsible for constructing DAG, and does instruction selection on
/// the DAG, eventually emits SDNodes of the DAG to LLVM IRs.
class RISCV32SelectionDAGISel : public RISCV32RaiserBase {
public:
  RISCV32SelectionDAGISel(RISCV32ModuleRaiser &mr);
  ~RISCV32SelectionDAGISel() override;
  void init(MachineFunction *mf = nullptr, Function *rf = nullptr) override;
  bool doSelection();
  bool runOnMachineFunction(MachineFunction &mf) override;
  bool setjtList(std::vector<JumpTableInfo> &List);
  static char ID;

private:
  void initEntryBasicBlock();
  void selectBasicBlock();
  void doInstructionSelection();
  void emitDAG();

  std::unique_ptr<OptimizationRemarkEmitter> ORE;

  RISCVFunctionRaisingInfo *FuncInfo;
  RISCVDAGBuilder *SDB;
  RISCVInstSelector *SLT;

  SelectionDAG *CurDAG;
  RISCVDAGRaisingInfo *DAGInfo;
  MachineBasicBlock *MBB;
  BasicBlock *BB;
  std::vector<JumpTableInfo> jtList;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32SELECTIONDAGISEL_H
