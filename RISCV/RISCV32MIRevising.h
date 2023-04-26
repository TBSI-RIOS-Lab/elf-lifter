//===- RISCV32MIRevising.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCV32MIRevising class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32MIREVISING_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32MIREVISING_H

#include "RISCV32RaiserBase.h"
#include "MCInstRaiser.h"
#include "llvm/CodeGen/MachineInstr.h"

using namespace llvm;
using namespace object;

/// This class is use to revise information of each MachineInstr. Something
/// like size of operands, immediate data value and so on. Currently, the
/// generation of global objects is included at here.
class RISCV32MIRevising : public RISCV32RaiserBase {

public:
  static char ID;
                    
  RISCV32MIRevising(RISCV32ModuleRaiser &MRsr);
  ~RISCV32MIRevising() override;
  void init(MachineFunction *mf = nullptr, Function *rf = nullptr) override;
  bool revise();
  bool runOnMachineFunction(MachineFunction &mf) override;
  void setMCInstRaiser(MCInstRaiser *PMCIR);
  void setGPBase(int64_t GP);
  int64_t getGPBase();
  void DeriveGP(MCInst &MInst, MCInst &NInst, int64_t SymAddr);
  void setGPFlag();
  bool GP_Derived();
  void FindLoadGP();
  int reviseMIforConst(MachineInstr &MInst);

private:
  bool reviseMI(MachineInstr &MI);
  /// Remove some useless operations of instructions.
  bool removeNeedlessInst(MachineInstr *MInst);
  /// Create function for external function.
  uint64_t getCalledFunctionAtPLTOffset(uint64_t PLTEndOff, uint64_t CallAddr);
  /// Relocate call branch instructions in object files.
  void relocateBranch(MachineInstr &MInst);
  /// Address PC relative data in function, and create corresponding global
  /// value.
  void addressPCRelativeData(MachineInstr &MInst);
  void addressPCRelativeConst(MachineInstr &MInst, MachineInstr &NInst);
  /// Decode modified immediate constants in some instructions with immediate
  /// operand.
  void decodeModImmOperand(MachineInstr &MInst);
  /// Find global value by PC offset.
  const Value *getGlobalValueByOffset(MachineInstr &MInst, int64_t PCOffset, int64_t flag);

  MCInstRaiser *MCIR;
  // int64_t _GPbase;
  int64_t _GP_flag;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32MIREVISING_H
