//===- RISCV32CreateJumpTable.h - Binary raiser utility llvm-mctoll -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCV32CreateJumpTable
// class for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32CREATEJUMPTABLE_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32CREATEJUMPTABLE_H

#include "RISCV32RaiserBase.h"
#include "MCInstRaiser.h"
#include "MachineFunctionRaiser.h"

class RISCV32CreateJumpTable : public RISCV32RaiserBase {
public:
  static char ID;

  RISCV32CreateJumpTable(RISCV32ModuleRaiser &mr);
  ~RISCV32CreateJumpTable() override;
  void init(MachineFunction *mf = nullptr, Function *rf = nullptr) override;
  bool create();
  bool runOnMachineFunction(MachineFunction &mf) override;
  bool getJTlist(std::vector<JumpTableInfo> &List);
  void setMCInstRaiser(MCInstRaiser *PMCIR);

private:
  unsigned int getRISCV32CPSR(unsigned int PhysReg);
  bool raiseMaichineJumpTable(MachineFunction &MF);
  /// Get the MachineBasicBlock to add the jumptable instruction.
  MachineBasicBlock *checkJumptableBB(MachineFunction &MF);
  bool UpdatetheBranchInst(MachineBasicBlock &MBB);

  std::vector<JumpTableInfo> jtList;
  MCInstRaiser *MCIR;
};
#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32CREATEJUMPTABLE_H
