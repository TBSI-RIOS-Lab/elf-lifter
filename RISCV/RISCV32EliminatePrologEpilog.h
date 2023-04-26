//===-- RISCV32EliminatePrologEpilog.h ------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCV32EliminatePrologEpilog class for
// use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32ELIMINATEPROLOGEPILOG_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32ELIMINATEPROLOGEPILOG_H

#include "RISCV32RaiserBase.h"

using namespace llvm;

class RISCV32EliminatePrologEpilog : public RISCV32RaiserBase {
public:
  static char ID;

  RISCV32EliminatePrologEpilog(RISCV32ModuleRaiser &mr);
  ~RISCV32EliminatePrologEpilog();
  void init(MachineFunction *mf = nullptr, Function *rf = nullptr) override;
  bool eliminate();
  bool runOnMachineFunction(MachineFunction &mf) override;

private:
  bool checkRegister(unsigned Reg, std::vector<MachineInstr *> &instrs) const;
  bool eliminateProlog(MachineFunction &mf) const;
  bool eliminateEpilog(MachineFunction &mf) const;
  /// Analyze stack size base on moving sp.
  void analyzeStackSize(MachineFunction &mf);
  /// Analyze frame adjustment base on the offset between fp and base sp.
  void analyzeFrameAdjustment(MachineFunction &mf);
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32ELIMINATEPROLOGEPILOG_H
