//===- SelectionCommon.h ----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains some declarations, and defines some structures, which are
// use to DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_SELECTIONCOMMON_H
#define LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_SELECTIONCOMMON_H

#include "RISCVISelLowering.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"

/// This is the start index of EXT_RISCV32ISD. Because node types which start
/// from RISCV32ISD::VLD1DUP (Next to RISCV32ISD::MEMCPY) are identified as
/// TARGET_MEMORY_OPCODE, we set EXTRISCV32ISD_OP_BEGIN index after RISCV32ISD::MEMCPY,
/// plugs 40 to keep long time with no confliction.
// #define EXTRISCV32ISD_OP_BEGIN (RISCVISD::MEMCPY + 40)

namespace llvm {
namespace EXT_RISCV32ISD {

enum NodeType {
  // BX_RET = EXTRISCV32ISD_OP_BEGIN,
  LOAD=30234,
  BRD, // Direct branch
  STORE,
  MSR,
  MRS,
  RSB,
  RSC,
  SBC,
  TEQ,
  TST,
  BIC,
  MLA,
  UXTB,
  CMOV,
  BGE,
  BLE,
  BGT,
  BLT,
  BNE,
  EXT_RISCV32ISD_OP_END
};

} // namespace EXT_RISCV32ISD
} // namespace llvm

using namespace llvm;

/// This structure is to extend SDNode properties, some additional SDNode
/// properties which are used by llvm-mctoll will be kept at here.
typedef struct NodeProperty {
  bool HasCPSR;
  bool Special;
  bool UpdateCPSR;
  unsigned Cond;
  bool IsTwoAddress;
  Value *Val;
  const MachineInstr *MI;
} NodePropertyInfo;

#endif // LLVM_TOOLS_LLVM_MCTOLL_RISCV32_DAG_SELECTIONCOMMON_H
