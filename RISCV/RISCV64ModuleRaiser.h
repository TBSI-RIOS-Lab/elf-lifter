//===-- RISCV32ModuleRaiser.h ---------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCV32ModuleRaiser class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV64_RISCV64MODULERAISER_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV64_RISCV64MODULERAISER_H

#include "ModuleRaiser.h"

using namespace llvm;

class RISCV64ModuleRaiser : public ModuleRaiser {
public:
  RISCV64ModuleRaiser() : ModuleRaiser() { Arch = Triple::riscv64; };

  MachineFunctionRaiser *
  CreateAndAddMachineFunctionRaiser(Function *F, const ModuleRaiser *MR,
                                    uint64_t Start, uint64_t End);
  bool collectDynamicRelocations();
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV64_RISCV64MODULERAISER_H
