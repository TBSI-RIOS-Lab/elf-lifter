//===- RISCV32FrameBuilder.h ----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCV32FrameBuilder class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32FRAMEBUILDER_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32FRAMEBUILDER_H

#include "RISCV32RaiserBase.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/IR/DataLayout.h"

using namespace llvm;

/// This class is use to build RISCV32 abstract stack frame by analyzing RISCV32 SP
/// register operations. Simultaneously, converts MI SP operands to
/// MO_FrameIndex type.
class RISCV32FrameBuilder : public RISCV32RaiserBase {
private:
  struct StackElement {
    uint64_t Size;
    int64_t SPOffset;
    int64_t ObjectIndex = -1; // If it is -1, means the corresponding
                              // StackObject has not been created.
  };

  MachineFrameInfo *MFI;
  LLVMContext *CTX;
  const DataLayout *DLT;

public:
  static char ID;

  RISCV32FrameBuilder(RISCV32ModuleRaiser &mr);
  ~RISCV32FrameBuilder() override;
  void init(MachineFunction *mf = nullptr, Function *rf = nullptr) override;
  bool build();
  bool runOnMachineFunction(MachineFunction &mf) override;

private:
  unsigned getBitCount(unsigned opcode);
  Type *getStackType(unsigned size);
  /// Replace common regs assigned by SP to SP.
  bool replaceNonSPBySP(MachineInstr &mi);
  /// Analyze frame index of stack operands.
  int64_t identifyStackOp(const MachineInstr &mi);
  /// Find out all of frame relative operands, and update them.
  void searchStackObjects(MachineFunction &mf);

  /// Records of assigned common registers by sp.
  SmallVector<unsigned, 16> RegAssignedBySP;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_RISCV32FRAMEBUILDER_H
